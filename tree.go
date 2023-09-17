package chi

// Radix tree implementation below is a based on the original work by
// Armon Dadgar in https://github.com/armon/go-radix/blob/master/radix.go
// (MIT licensed). It's been heavily modified for use as a HTTP routing tree.

import (
	"fmt"
	"net/http"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

// http方法类型
type methodTyp uint

const (
	mSTUB methodTyp = 1 << iota
	mCONNECT
	mDELETE
	mGET
	mHEAD
	mOPTIONS
	mPATCH
	mPOST
	mPUT
	mTRACE
)

var mALL = mCONNECT | mDELETE | mGET | mHEAD |
	mOPTIONS | mPATCH | mPOST | mPUT | mTRACE

var methodMap = map[string]methodTyp{
	http.MethodConnect: mCONNECT,
	http.MethodDelete:  mDELETE,
	http.MethodGet:     mGET,
	http.MethodHead:    mHEAD,
	http.MethodOptions: mOPTIONS,
	http.MethodPatch:   mPATCH,
	http.MethodPost:    mPOST,
	http.MethodPut:     mPUT,
	http.MethodTrace:   mTRACE,
}

var reverseMethodMap = map[methodTyp]string{
	mCONNECT: http.MethodConnect,
	mDELETE:  http.MethodDelete,
	mGET:     http.MethodGet,
	mHEAD:    http.MethodHead,
	mOPTIONS: http.MethodOptions,
	mPATCH:   http.MethodPatch,
	mPOST:    http.MethodPost,
	mPUT:     http.MethodPut,
	mTRACE:   http.MethodTrace,
}

// RegisterMethod adds support for custom HTTP method handlers, available
// via Router#Method and Router#MethodFunc
func RegisterMethod(method string) {
	if method == "" {
		return
	}
	method = strings.ToUpper(method)
	if _, ok := methodMap[method]; ok {
		return
	}
	n := len(methodMap)
	if n > strconv.IntSize-2 {
		panic(fmt.Sprintf("chi: max number of methods reached (%d)", strconv.IntSize))
	}
	mt := methodTyp(2 << n)
	methodMap[method] = mt
	mALL |= mt
}

// 节点类型
type nodeTyp uint8

const (
	ntStatic   nodeTyp = iota // 静态节点，例如 /home
	ntRegexp                  // 正则表达式节点，例如 /{id:[0-9]+}
	ntParam                   // 参数节点，例如 /{user}
	ntCatchAll                // 捕获所有的节点，例如 /api/v1/*
)

// 节点
type node struct {
	// subroutes on the leaf node
	// 存储叶子节点上的子路由
	subroutes Routes

	// regexp matcher for regexp nodes
	// 存储正则表达式节点的匹配器
	rex *regexp.Regexp

	// HTTP handler endpoints on the leaf node
	// 存储叶子节点上的HTTP处理器端点
	endpoints endpoints

	// prefix is the common prefix we ignore
	// 忽略的公共前缀
	prefix string

	// child nodes should be stored in-order for iteration,
	// in groups of the node type.
	// 存储子节点。子节点应该按照节点类型的顺序存储
	//
	// 静态节点，例如 /home
	// 正则表达式节点，例如 /{id:[0-9]+}
	// 参数节点，例如 /{user}
	// 捕获所有的节点，例如 /api/v1/*
	children [ntCatchAll + 1]nodes // 声明一个长度为ntCatchAll+1的[]*node

	// first byte of the child prefix
	// 子前缀的第一个字节
	tail byte

	// node type: static, regexp, param, catchAll
	// 节点类型：静态、正则表达式、参数、捕获所有
	typ nodeTyp

	// first byte of the prefix
	// 这是一个字节，表示前缀的第一个字节
	label byte
}

// endpoints is a mapping of http method constants to handlers
// for a given route.
// 这是一个映射类型，键是methodTyp类型，值是指向endpoint类型的指针。这个类型用于给定路由的HTTP方法到处理器的映射。
type endpoints map[methodTyp]*endpoint // 路由一摸一样，只是http方法类型不一样 key=http方法类型 value=处理函数信息

type endpoint struct {
	// endpoint handler
	// 这是一个http.Handler类型的字段，表示端点处理器。http.Handler是一个接口，任何实现了ServeHTTP(ResponseWriter, *Request)方法的类型都实现了这个接口，可以作为HTTP处理器。
	handler http.Handler

	// pattern is the routing pattern for handler nodes
	// 表示处理器节点的路由模式。
	pattern string

	// parameter keys recorded on handler nodes
	// 记录了处理器节点上的参数键。
	paramKeys []string
}

func (s endpoints) Value(method methodTyp) *endpoint {
	mh, ok := s[method]
	if !ok {
		mh = &endpoint{}
		s[method] = mh
	}
	return mh
}

// InsertRoute
// 根据输入的路由模式（pattern）在路由树中寻找或创建对应的节点，并将HTTP处理器（handler）和HTTP方法（method）设置到对应的节点上。
//
// 三个参数：
// method（HTTP方法类型，如GET、POST等），
// pattern（路由模式，如"/users/{id}"），
// handler（处理该路由的HTTP处理器）。这个方法绑定在node类型上，返回一个node类型的指针。
func (n *node) InsertRoute(method methodTyp, pattern string, handler http.Handler) *node {
	var parent *node  //
	search := pattern //

	for {
		// 如果搜索的路由模式（search）为空，说明已经处理完所有的路由段，将HTTP处理器和HTTP方法设置到当前节点上，并返回当前节点。
		// Handle key exhaustion
		if len(search) == 0 {
			// Insert or update the node's leaf handler
			n.setEndpoint(method, handler, pattern)
			return n
		}

		// 如果搜索的路由模式的第一个字符是'{'或'*'，说明这是一个参数或通配符节点，使用patNextSegment函数解析出路由段的类型、正则表达式模式、尾部字符、结束索引等信息。
		// We're going to be searching for a wild node next,
		// in this case, we need to get the tail
		var label = search[0] // 取搜索的路由模式的第一个字符作为标签

		var segTyp nodeTyp   // 节点类型
		var segRexpat string // 正则表达式模式
		var segTail byte     // 参数尾部字符
		var segEndIdx int    // 参数结束索引
		if label == '{' || label == '*' {
			segTyp, _, segRexpat, segTail, _, segEndIdx = patNextSegment(search)
		}

		var prefix string
		if segTyp == ntRegexp { // 正则表达式节点
			prefix = segRexpat
		}

		// 在当前节点的子节点中查找与路由段匹配的节点。如果找不到匹配的子节点，函数会创建一个新的子节点，并将HTTP处理器和HTTP方法设置到新的子节点上，然后返回新的子节点。
		// Look for the edge to attach to
		parent = n
		n = n.getEdge(segTyp, label, segTail, prefix)

		// No edge, create one
		if n == nil {
			child := &node{label: label, tail: segTail, prefix: search}
			hn := parent.addChild(child, search)
			hn.setEndpoint(method, handler, pattern)
			return hn
		}

		// 找到了匹配的子节点，函数会根据子节点的类型进行处理。
		// Found an edge to match the pattern

		// 子节点是参数或通配符节点，函数会从搜索的路由模式中移除已匹配的部分，然后继续下一轮循环。
		if n.typ > ntStatic {
			// We found a param node, trim the param from the search path and continue.
			// This param/wild pattern segment would already be on the tree from a previous
			// call to addChild when creating a new node.
			search = search[segEndIdx:]
			continue
		}

		// 子节点是静态节点，函数会计算搜索的路由模式和子节点前缀的最长公共前缀，然后根据最长公共前缀的长度进行处理。
		// 最长公共前缀的长度等于子节点前缀的长度，函数会从搜索的路由模式中移除已匹配的部分，然后继续下一轮循环。
		// Static nodes fall below here.
		// Determine longest prefix of the search key on match.
		commonPrefix := longestPrefix(search, n.prefix)
		if commonPrefix == len(n.prefix) {
			// the common prefix is as long as the current node's prefix we're attempting to insert.
			// keep the search going.
			search = search[commonPrefix:]
			continue
		}

		// 最长公共前缀的长度小于子节点前缀的长度，函数会创建一个新的子节点，类型是静态，前缀是search和子节点前缀的最长公共前缀。
		// Split the node
		child := &node{
			typ:    ntStatic,
			prefix: search[:commonPrefix],
		}
		// 在parent节点中替换子节点，并更新找到的子节点的标签和前缀。接着，函数会将找到的子节点添加到新的子节点下。
		parent.replaceChild(search[0], segTail, child)

		// Restore the existing node
		n.label = n.prefix[commonPrefix]
		n.prefix = n.prefix[commonPrefix:]
		child.addChild(n, n.prefix)

		// 函数会从search中移除已匹配的部分。如果search的长度为0，说明已经处理完所有的路由段，函数会将HTTP处理器和HTTP方法设置到新的子节点上，并返回新的子节点。
		// If the new key is a subset, set the method/handler on this node and finish.
		search = search[commonPrefix:]
		if len(search) == 0 {
			child.setEndpoint(method, handler, pattern)
			return child
		}

		// search的长度不为0，函数会创建一个新的子节点，类型是静态，标签是search的第一个字符，前缀是search，然后将新的子节点添加到新的子节点下，并将HTTP处理器和HTTP方法设置到新的子节点上，然后返回新的子节点。
		// Create a new edge for the node
		subchild := &node{
			typ:    ntStatic,
			label:  search[0],
			prefix: search,
		}
		hn := child.addChild(subchild, search)
		hn.setEndpoint(method, handler, pattern)
		return hn
	}
}

// addChild appends the new `child` node to the tree using the `pattern` as the trie key.
// For a URL router like chi's, we split the static, param, regexp and wildcard segments
// into different nodes. In addition, addChild will recursively call itself until every
// pattern segment is added to the url pattern tree as individual nodes, depending on type.
func (n *node) addChild(child *node, prefix string) *node {
	search := prefix

	// handler leaf node added to the tree is the child.
	// this may be overridden later down the flow
	hn := child

	// Parse next segment
	segTyp, _, segRexpat, segTail, segStartIdx, segEndIdx := patNextSegment(search)

	// Add child depending on next up segment
	switch segTyp {

	case ntStatic:
		// Search prefix is all static (that is, has no params in path)
		// noop

	default:
		// Search prefix contains a param, regexp or wildcard

		if segTyp == ntRegexp {
			rex, err := regexp.Compile(segRexpat)
			if err != nil {
				panic(fmt.Sprintf("chi: invalid regexp pattern '%s' in route param", segRexpat))
			}
			child.prefix = segRexpat
			child.rex = rex
		}

		if segStartIdx == 0 {
			// Route starts with a param
			child.typ = segTyp

			if segTyp == ntCatchAll {
				segStartIdx = -1
			} else {
				segStartIdx = segEndIdx
			}
			if segStartIdx < 0 {
				segStartIdx = len(search)
			}
			child.tail = segTail // for params, we set the tail

			if segStartIdx != len(search) {
				// add static edge for the remaining part, split the end.
				// its not possible to have adjacent param nodes, so its certainly
				// going to be a static node next.

				search = search[segStartIdx:] // advance search position

				nn := &node{
					typ:    ntStatic,
					label:  search[0],
					prefix: search,
				}
				hn = child.addChild(nn, search)
			}

		} else if segStartIdx > 0 {
			// Route has some param

			// starts with a static segment
			child.typ = ntStatic
			child.prefix = search[:segStartIdx]
			child.rex = nil

			// add the param edge node
			search = search[segStartIdx:]

			nn := &node{
				typ:   segTyp,
				label: search[0],
				tail:  segTail,
			}
			hn = child.addChild(nn, search)

		}
	}

	n.children[child.typ] = append(n.children[child.typ], child)
	n.children[child.typ].Sort()
	return hn
}

func (n *node) replaceChild(label, tail byte, child *node) {
	for i := 0; i < len(n.children[child.typ]); i++ {
		if n.children[child.typ][i].label == label && n.children[child.typ][i].tail == tail {
			n.children[child.typ][i] = child
			n.children[child.typ][i].label = label
			n.children[child.typ][i].tail = tail
			return
		}
	}
	panic("chi: replacing missing child")
}

func (n *node) getEdge(ntyp nodeTyp, label, tail byte, prefix string) *node {
	nds := n.children[ntyp]
	for i := 0; i < len(nds); i++ {
		if nds[i].label == label && nds[i].tail == tail {
			if ntyp == ntRegexp && nds[i].prefix != prefix {
				continue
			}
			return nds[i]
		}
	}
	return nil
}

func (n *node) setEndpoint(method methodTyp, handler http.Handler, pattern string) {
	// Set the handler for the method type on the node
	if n.endpoints == nil {
		n.endpoints = make(endpoints)
	}

	paramKeys := patParamKeys(pattern)

	if method&mSTUB == mSTUB {
		n.endpoints.Value(mSTUB).handler = handler
	}
	if method&mALL == mALL {
		h := n.endpoints.Value(mALL)
		h.handler = handler
		h.pattern = pattern
		h.paramKeys = paramKeys
		for _, m := range methodMap {
			h := n.endpoints.Value(m)
			h.handler = handler
			h.pattern = pattern
			h.paramKeys = paramKeys
		}
	} else {
		h := n.endpoints.Value(method)
		h.handler = handler
		h.pattern = pattern
		h.paramKeys = paramKeys
	}
}

// 查找路由
func (n *node) FindRoute(rctx *Context, method methodTyp, path string) (*node, endpoints, http.Handler) {
	// Reset the context routing pattern and params
	rctx.routePattern = ""
	rctx.routeParams.Keys = rctx.routeParams.Keys[:0]
	rctx.routeParams.Values = rctx.routeParams.Values[:0]

	// Find the routing handlers for the path
	rn := n.findRoute(rctx, method, path)
	if rn == nil {
		return nil, nil, nil
	}

	// Record the routing params in the request lifecycle
	rctx.URLParams.Keys = append(rctx.URLParams.Keys, rctx.routeParams.Keys...)
	rctx.URLParams.Values = append(rctx.URLParams.Values, rctx.routeParams.Values...)

	// Record the routing pattern in the request lifecycle
	if rn.endpoints[method].pattern != "" {
		rctx.routePattern = rn.endpoints[method].pattern
		rctx.RoutePatterns = append(rctx.RoutePatterns, rctx.routePattern)
	}

	return rn, rn.endpoints, rn.endpoints[method].handler
}

// Recursive edge traversal by checking all nodeTyp groups along the way.
// It's like searching through a multi-dimensional radix trie.
//
// 在路由树中查找匹配的路由
// 三个参数：一个路由上下文rctx，一个HTTP方法method，和一个路径path。
func (n *node) findRoute(rctx *Context, method methodTyp, path string) *node {
	nn := n
	search := path

	// 遍历当前节点的所有子节点，根据子节点的类型进行不同的处理
	for t, nds := range nn.children {
		// 节点类型
		ntyp := nodeTyp(t)
		if len(nds) == 0 {
			continue
		}

		var xn *node
		xsearch := search

		var label byte
		if search != "" {
			label = search[0]
		}

		switch ntyp {
		// 子节点是静态类型（ntStatic），则查找与路径首字符匹配的子节点。如果找到匹配的子节点，并且该子节点的前缀是路径的前缀，则更新搜索路径，剔除已匹配的前缀部分。
		case ntStatic:
			xn = nds.findEdge(label)
			if xn == nil || !strings.HasPrefix(xsearch, xn.prefix) {
				continue
			}
			xsearch = xsearch[len(xn.prefix):]
		// 子节点是参数类型（ntParam）或正则表达式类型（ntRegexp），则查找路径中与子节点尾部分隔符匹配的部分。如果找到匹配的部分，并且该部分满足正则表达式或不包含'/'字符，则将该部分添加到路由参数中，并更新搜索路径，剔除已匹配的部分。
		case ntParam, ntRegexp:
			// short-circuit and return no matching route for empty param values
			if xsearch == "" {
				continue
			}

			// serially loop through each node grouped by the tail delimiter
			for idx := 0; idx < len(nds); idx++ {
				xn = nds[idx]

				// label for param nodes is the delimiter byte
				p := strings.IndexByte(xsearch, xn.tail)

				if p < 0 {
					if xn.tail == '/' {
						p = len(xsearch)
					} else {
						continue
					}
				} else if ntyp == ntRegexp && p == 0 {
					continue
				}

				if ntyp == ntRegexp && xn.rex != nil {
					if !xn.rex.MatchString(xsearch[:p]) {
						continue
					}
				} else if strings.IndexByte(xsearch[:p], '/') != -1 {
					// avoid a match across path segments
					continue
				}

				prevlen := len(rctx.routeParams.Values)
				rctx.routeParams.Values = append(rctx.routeParams.Values, xsearch[:p])
				xsearch = xsearch[p:]

				if len(xsearch) == 0 {
					if xn.isLeaf() {
						h := xn.endpoints[method]
						if h != nil && h.handler != nil {
							rctx.routeParams.Keys = append(rctx.routeParams.Keys, h.paramKeys...)
							return xn
						}

						for endpoints := range xn.endpoints {
							if endpoints == mALL || endpoints == mSTUB {
								continue
							}
							rctx.methodsAllowed = append(rctx.methodsAllowed, endpoints)
						}

						// flag that the routing context found a route, but not a corresponding
						// supported method
						rctx.methodNotAllowed = true
					}
				}

				// recursively find the next node on this branch
				fin := xn.findRoute(rctx, method, xsearch)
				if fin != nil {
					return fin
				}

				// not found on this branch, reset vars
				rctx.routeParams.Values = rctx.routeParams.Values[:prevlen]
				xsearch = search
			}

			rctx.routeParams.Values = append(rctx.routeParams.Values, "")

		default:
			// 子节点是其他类型，则将整个路径添加到路由参数中，并将搜索路径设置为空。
			// catch-all nodes
			rctx.routeParams.Values = append(rctx.routeParams.Values, search)
			xn = nds[0]
			xsearch = ""
		}

		if xn == nil {
			continue
		}

		// 如果搜索路径为空，且子节点是叶子节点，且子节点有对应的处理函数，则将子节点的参数键添加到路由参数中，并返回子节点。
		// did we find it yet?
		if len(xsearch) == 0 {
			if xn.isLeaf() {
				h := xn.endpoints[method]
				if h != nil && h.handler != nil {
					rctx.routeParams.Keys = append(rctx.routeParams.Keys, h.paramKeys...)
					return xn
				}

				for endpoints := range xn.endpoints {
					if endpoints == mALL || endpoints == mSTUB {
						continue
					}
					rctx.methodsAllowed = append(rctx.methodsAllowed, endpoints)
				}

				// flag that the routing context found a route, but not a corresponding
				// supported method
				rctx.methodNotAllowed = true
			}
		}

		// 搜索路径不为空，或子节点不是叶子节点，或子节点没有对应的处理函数，则递归调用findRoute函数，继续在子节点的子节点中查找匹配的路由。
		// recursively find the next node..
		fin := xn.findRoute(rctx, method, xsearch)
		if fin != nil {
			return fin
		}

		// Did not find final handler, let's remove the param here if it was set
		if xn.typ > ntStatic {
			if len(rctx.routeParams.Values) > 0 {
				rctx.routeParams.Values = rctx.routeParams.Values[:len(rctx.routeParams.Values)-1]
			}
		}

	}

	// 在所有子节点中都没有找到匹配的路由，则返回nil。
	return nil
}

func (n *node) findEdge(ntyp nodeTyp, label byte) *node {
	nds := n.children[ntyp]
	num := len(nds)
	idx := 0

	switch ntyp {
	case ntStatic, ntParam, ntRegexp:
		i, j := 0, num-1
		for i <= j {
			idx = i + (j-i)/2
			if label > nds[idx].label {
				i = idx + 1
			} else if label < nds[idx].label {
				j = idx - 1
			} else {
				i = num // breaks cond
			}
		}
		if nds[idx].label != label {
			return nil
		}
		return nds[idx]

	default: // catch all
		return nds[idx]
	}
}

func (n *node) isLeaf() bool {
	return n.endpoints != nil
}

func (n *node) findPattern(pattern string) bool {
	nn := n
	for _, nds := range nn.children {
		if len(nds) == 0 {
			continue
		}

		n = nn.findEdge(nds[0].typ, pattern[0])
		if n == nil {
			continue
		}

		var idx int
		var xpattern string

		switch n.typ {
		case ntStatic:
			idx = longestPrefix(pattern, n.prefix)
			if idx < len(n.prefix) {
				continue
			}

		case ntParam, ntRegexp:
			idx = strings.IndexByte(pattern, '}') + 1

		case ntCatchAll:
			idx = longestPrefix(pattern, "*")

		default:
			panic("chi: unknown node type")
		}

		xpattern = pattern[idx:]
		if len(xpattern) == 0 {
			return true
		}

		return n.findPattern(xpattern)
	}
	return false
}

func (n *node) routes() []Route {
	rts := []Route{}

	n.walk(func(eps endpoints, subroutes Routes) bool {
		if eps[mSTUB] != nil && eps[mSTUB].handler != nil && subroutes == nil {
			return false
		}

		// Group methodHandlers by unique patterns
		pats := make(map[string]endpoints)

		for mt, h := range eps {
			if h.pattern == "" {
				continue
			}
			p, ok := pats[h.pattern]
			if !ok {
				p = endpoints{}
				pats[h.pattern] = p
			}
			p[mt] = h
		}

		for p, mh := range pats {
			hs := make(map[string]http.Handler)
			if mh[mALL] != nil && mh[mALL].handler != nil {
				hs["*"] = mh[mALL].handler
			}

			for mt, h := range mh {
				if h.handler == nil {
					continue
				}
				m := methodTypString(mt)
				if m == "" {
					continue
				}
				hs[m] = h.handler
			}

			rt := Route{subroutes, hs, p}
			rts = append(rts, rt)
		}

		return false
	})

	return rts
}

func (n *node) walk(fn func(eps endpoints, subroutes Routes) bool) bool {
	// Visit the leaf values if any
	if (n.endpoints != nil || n.subroutes != nil) && fn(n.endpoints, n.subroutes) {
		return true
	}

	// Recurse on the children
	for _, ns := range n.children {
		for _, cn := range ns {
			if cn.walk(fn) {
				return true
			}
		}
	}
	return false
}

// patNextSegment returns the next segment details from a pattern:
// node type, param key, regexp string, param tail byte, param starting index, param ending index
//
// 这个函数的主要目的是解析路由模式中的下一个路由段，并返回路由段的详细信息，包括节点类型、参数键、正则表达式模式、参数尾部字符、参数开始索引和参数结束索引。
//
// 假设我们有以下路由模式：
// 1. "/users/{userID}"
// 2. "/users/{userID:[0-9]+}"
// 3. "/files/*"
//
// 对于第一个路由模式，我们可以调用`patNextSegment("/users/{userID}")`，函数会返回以下结果：
// - 节点类型：ntParam（表示这是一个参数节点）
// - 参数键："userID"
// - 正则表达式模式：""
// - 参数尾部字符：'/'
// - 参数开始索引：7（'{'字符在路由模式中的位置）
// - 参数结束索引：15（'}'字符在路由模式中的位置）
//
// 对于第二个路由模式，我们可以调用`patNextSegment("/users/{userID:[0-9]+}")`，函数会返回以下结果：
// - 节点类型：ntRegexp（表示这是一个正则表达式节点）
// - 参数键："userID"
// - 正则表达式模式："^[0-9]+$"（函数会自动添加'^'和'$'）
// - 参数尾部字符：'/'
// - 参数开始索引：6（'{'字符在路由模式中的位置）
// - 参数结束索引：20（'}'字符在路由模式中的位置）
//
// 对于第三个路由模式，我们可以调用`patNextSegment("/files/*")`，函数会返回以下结果：
// - 节点类型：ntCatchAll（表示这是一个通配符节点）
// - 参数键："*"
// - 正则表达式模式：""
// - 参数尾部字符：0
// - 参数开始索引：7（'*'字符在路由模式中的位置）
// - 参数结束索引：8（路由模式的长度）
//
// 这些结果可以用于在路由树中插入新的路由。
func patNextSegment(pattern string) (nodeTyp, string, string, byte, int, int) {
	// 查找路由模式中的'{'和'*'字符的位置。如果找不到这两个字符，说明这是一个静态节点，函数会返回静态节点类型和整个路由模式。
	ps := strings.Index(pattern, "{")
	ws := strings.Index(pattern, "*")
	if ps < 0 && ws < 0 {
		return ntStatic, "", "", 0, 0, len(pattern) // we return the entire thing
	}

	// 检查'{'和''字符的位置。如果''字符在'{'字符之前，函数会抛出一个panic，因为在路由模式中，通配符节点必须是最后一个节点。
	// Sanity check
	if ps >= 0 && ws >= 0 && ws < ps {
		panic("chi: wildcard '*' must be the last pattern in a route, otherwise use a '{param}'")
	}

	// 设置默认的参数尾部字符为'/'。
	var tail byte = '/' // Default endpoint tail to / byte

	// 找到了'{'字符，说明这是一个参数节点或正则表达式节点。
	// 参数节点 /{user}
	// 正则表达式节点 /{id:[0-9]+}
	if ps >= 0 {
		// Param/Regexp pattern is next
		// 参数节点
		nt := ntParam

		// 会查找与'{'字符匹配的'}'字符，并检查是否存在嵌套的'{'和'}'字符。如果找不到匹配的'}'字符，函数会抛出一个panic。
		// Read to closing } taking into account opens and closes in curl count (cc)
		cc := 0
		pe := ps
		for i, c := range pattern[ps:] {
			if c == '{' {
				cc++
			} else if c == '}' {
				cc--
				if cc == 0 {
					pe = ps + i
					break
				}
			}
		}
		if pe == ps {
			panic("chi: route param closing delimiter '}' is missing")
		}

		// 提取出参数键，并检查参数键中是否包含':'字符。如果包含':'字符，说明这是一个正则表达式节点，函数会提取出正则表达式模式，并检查正则表达式模式是否以'^'开始和'$'结束，如果不是，函数会添加'^'和'$'。
		key := pattern[ps+1 : pe]
		pe++ // set end to next position

		if pe < len(pattern) {
			tail = pattern[pe]
		}

		var rexpat string
		if idx := strings.Index(key, ":"); idx >= 0 {
			nt = ntRegexp // 正则表达式节点
			rexpat = key[idx+1:]
			key = key[:idx]
		}

		if len(rexpat) > 0 {
			if rexpat[0] != '^' {
				rexpat = "^" + rexpat
			}
			if rexpat[len(rexpat)-1] != '$' {
				rexpat += "$"
			}
		}

		// 返回节点类型、参数键、正则表达式模式、参数尾部字符、参数开始索引和参数结束索引。
		return nt, key, rexpat, tail, ps, pe
	}

	// 如果找到了''字符，说明这是一个通配符节点。函数会检查''字符是否是路由模式的最后一个字符，如果不是，函数会抛出一个panic。
	// 然后，函数会返回通配符节点类型、''字符、空字符串、0、''字符的位置和路由模式的长度。
	// Wildcard pattern as finale
	if ws < len(pattern)-1 {
		panic("chi: wildcard '*' must be the last value in a route. trim trailing text or use a '{param}' instead")
	}
	return ntCatchAll, "*", "", 0, ws, len(pattern)
}

func patParamKeys(pattern string) []string {
	pat := pattern
	paramKeys := []string{}
	for {
		ptyp, paramKey, _, _, _, e := patNextSegment(pat)
		if ptyp == ntStatic {
			return paramKeys
		}
		for i := 0; i < len(paramKeys); i++ {
			if paramKeys[i] == paramKey {
				panic(fmt.Sprintf("chi: routing pattern '%s' contains duplicate param key, '%s'", pattern, paramKey))
			}
		}
		paramKeys = append(paramKeys, paramKey)
		pat = pat[e:]
	}
}

// longestPrefix finds the length of the shared prefix
// of two strings
func longestPrefix(k1, k2 string) int {
	max := len(k1)
	if l := len(k2); l < max {
		max = l
	}
	var i int
	for i = 0; i < max; i++ {
		if k1[i] != k2[i] {
			break
		}
	}
	return i
}

func methodTypString(method methodTyp) string {
	for s, t := range methodMap {
		if method == t {
			return s
		}
	}
	return ""
}

type nodes []*node

// Sort the list of nodes by label
func (ns nodes) Sort()              { sort.Sort(ns); ns.tailSort() }
func (ns nodes) Len() int           { return len(ns) }
func (ns nodes) Swap(i, j int)      { ns[i], ns[j] = ns[j], ns[i] }
func (ns nodes) Less(i, j int) bool { return ns[i].label < ns[j].label }

// tailSort pushes nodes with '/' as the tail to the end of the list for param nodes.
// The list order determines the traversal order.
func (ns nodes) tailSort() {
	for i := len(ns) - 1; i >= 0; i-- {
		if ns[i].typ > ntStatic && ns[i].tail == '/' {
			ns.Swap(i, len(ns)-1)
			return
		}
	}
}

func (ns nodes) findEdge(label byte) *node {
	num := len(ns)
	idx := 0
	i, j := 0, num-1
	for i <= j {
		idx = i + (j-i)/2
		if label > ns[idx].label {
			i = idx + 1
		} else if label < ns[idx].label {
			j = idx - 1
		} else {
			i = num // breaks cond
		}
	}
	if ns[idx].label != label {
		return nil
	}
	return ns[idx]
}

// Route describes the details of a routing handler.
// Handlers map key is an HTTP method
type Route struct {
	SubRoutes Routes
	Handlers  map[string]http.Handler
	Pattern   string
}

// WalkFunc is the type of the function called for each method and route visited by Walk.
type WalkFunc func(method string, route string, handler http.Handler, middlewares ...func(http.Handler) http.Handler) error

// Walk walks any router tree that implements Routes interface.
func Walk(r Routes, walkFn WalkFunc) error {
	return walk(r, walkFn, "")
}

func walk(r Routes, walkFn WalkFunc, parentRoute string, parentMw ...func(http.Handler) http.Handler) error {
	for _, route := range r.Routes() {
		mws := make([]func(http.Handler) http.Handler, len(parentMw))
		copy(mws, parentMw)
		mws = append(mws, r.Middlewares()...)

		if route.SubRoutes != nil {
			if err := walk(route.SubRoutes, walkFn, parentRoute+route.Pattern, mws...); err != nil {
				return err
			}
			continue
		}

		for method, handler := range route.Handlers {
			if method == "*" {
				// Ignore a "catchAll" method, since we pass down all the specific methods for each route.
				continue
			}

			fullRoute := parentRoute + route.Pattern
			fullRoute = strings.Replace(fullRoute, "/*/", "/", -1)

			if chain, ok := handler.(*ChainHandler); ok {
				if err := walkFn(method, fullRoute, chain.Endpoint, append(mws, chain.Middlewares...)...); err != nil {
					return err
				}
			} else {
				if err := walkFn(method, fullRoute, handler, mws...); err != nil {
					return err
				}
			}
		}
	}

	return nil
}
