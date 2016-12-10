(function (exports) {
'use strict';

var fableGlobal = function () {
    var globalObj = typeof window !== "undefined" ? window : typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : null;
    if (typeof globalObj.__FABLE_CORE__ === "undefined") {
        globalObj.__FABLE_CORE__ = {
            types: new Map(),
            symbols: {
                reflection: Symbol("reflection"),
                generics: Symbol("generics")
            }
        };
    }
    return globalObj.__FABLE_CORE__;
}();
function setType(fullName, cons) {
    fableGlobal.types.set(fullName, cons);
}
function getType(fullName) {
    return fableGlobal.types.get(fullName);
}
var _Symbol = fableGlobal.symbols;

var _createClass$1 = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

function _classCallCheck$1(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var NonDeclaredType = function () {
    function NonDeclaredType(kind, name, generics) {
        _classCallCheck$1(this, NonDeclaredType);

        this.kind = kind;
        this.name = name;
        this.generics = generics || [];
    }

    _createClass$1(NonDeclaredType, [{
        key: "Equals",
        value: function Equals(other) {
            return this.kind === other.kind && this.name === other.name && equals(this.generics, other.generics);
        }
    }]);

    return NonDeclaredType;
}();

var GenericNonDeclaredType = function (_NonDeclaredType) {
    _inherits(GenericNonDeclaredType, _NonDeclaredType);

    function GenericNonDeclaredType(kind, generics) {
        _classCallCheck$1(this, GenericNonDeclaredType);

        return _possibleConstructorReturn(this, (GenericNonDeclaredType.__proto__ || Object.getPrototypeOf(GenericNonDeclaredType)).call(this, kind, null, generics));
    }

    _createClass$1(GenericNonDeclaredType, [{
        key: _Symbol.generics,
        value: function value() {
            return this.generics;
        }
    }]);

    return GenericNonDeclaredType;
}(NonDeclaredType);

var Any = new NonDeclaredType("Any");
var Unit = new NonDeclaredType("Unit");

function Tuple(ts) {
    return new GenericNonDeclaredType("Tuple", ts);
}
function GenericParam(name) {
    return new NonDeclaredType("GenericParam", name);
}
function Interface(name) {
    return new NonDeclaredType("Interface", name);
}
function makeGeneric(typeDef, genArgs) {
    return function (_typeDef) {
        _inherits(_class, _typeDef);

        function _class() {
            _classCallCheck$1(this, _class);

            return _possibleConstructorReturn(this, (_class.__proto__ || Object.getPrototypeOf(_class)).apply(this, arguments));
        }

        _createClass$1(_class, [{
            key: _Symbol.generics,
            value: function value() {
                return genArgs;
            }
        }]);

        return _class;
    }(typeDef);
}
/**
 * Checks if this a function constructor extending another with generic info.
 */
function isGeneric(typ) {
    return typeof typ === "function" && !!typ.prototype[_Symbol.generics];
}
/**
 * Returns the parent if this is a declared generic type or the argument otherwise.
 * Attention: Unlike .NET this doesn't throw an exception if type is not generic.
*/
function getDefinition(typ) {
    return typeof typ === "function" && typ.prototype[_Symbol.generics] ? Object.getPrototypeOf(typ.prototype).constructor : typ;
}

function hasInterface(obj, interfaceName) {
    if (typeof obj[_Symbol.reflection] === "function") {
        var interfaces = obj[_Symbol.reflection]().interfaces;
        return Array.isArray(interfaces) && interfaces.indexOf(interfaceName) > -1;
    }
    return false;
}

function getRestParams(args, idx) {
    for (var _len = args.length, restArgs = Array(_len > idx ? _len - idx : 0), _key = idx; _key < _len; _key++) {
        restArgs[_key - idx] = args[_key];
    }return restArgs;
}
function toString(o) {
    return o != null && typeof o.ToString == "function" ? o.ToString() : String(o);
}

function equals(x, y) {
    // Optimization if they are referencially equal
    if (x === y) return true;else if (x == null) return y == null;else if (y == null) return false;else if (isGeneric(x) && isGeneric(y)) return getDefinition(x) === getDefinition(y) && equalsRecords(x.prototype[_Symbol.generics](), y.prototype[_Symbol.generics]());else if (Object.getPrototypeOf(x) !== Object.getPrototypeOf(y)) return false;else if (typeof x.Equals === "function") return x.Equals(y);else if (Array.isArray(x)) {
        if (x.length != y.length) return false;
        for (var i = 0; i < x.length; i++) {
            if (!equals(x[i], y[i])) return false;
        }return true;
    } else if (ArrayBuffer.isView(x)) {
        if (x.byteLength !== y.byteLength) return false;
        var dv1 = new DataView(x.buffer),
            dv2 = new DataView(y.buffer);
        for (var _i = 0; _i < x.byteLength; _i++) {
            if (dv1.getUint8(_i) !== dv2.getUint8(_i)) return false;
        }return true;
    } else if (x instanceof Date) return x.getTime() == y.getTime();else return false;
}
function compare(x, y) {
    // Optimization if they are referencially equal
    if (x === y) return 0;
    if (x == null) return y == null ? 0 : -1;else if (y == null) return -1;else if (Object.getPrototypeOf(x) !== Object.getPrototypeOf(y)) return -1;else if (hasInterface(x, "System.IComparable")) return x.CompareTo(y);else if (Array.isArray(x)) {
        if (x.length != y.length) return x.length < y.length ? -1 : 1;
        for (var i = 0, j = 0; i < x.length; i++) {
            if ((j = compare(x[i], y[i])) !== 0) return j;
        }return 0;
    } else if (ArrayBuffer.isView(x)) {
        if (x.byteLength != y.byteLength) return x.byteLength < y.byteLength ? -1 : 1;
        var dv1 = new DataView(x.buffer),
            dv2 = new DataView(y.buffer);
        for (var _i2 = 0, b1 = 0, b2 = 0; _i2 < x.byteLength; _i2++) {
            b1 = dv1.getUint8(_i2), b2 = dv2.getUint8(_i2);
            if (b1 < b2) return -1;
            if (b1 > b2) return 1;
        }
        return 0;
    } else if (x instanceof Date) return compare(x.getTime(), y.getTime());else return x < y ? -1 : 1;
}
function equalsRecords(x, y) {
    // Optimization if they are referencially equal
    if (x === y) {
        return true;
    } else {
        var keys = Object.getOwnPropertyNames(x);
        for (var i = 0; i < keys.length; i++) {
            if (!equals(x[keys[i]], y[keys[i]])) return false;
        }
        return true;
    }
}
function compareRecords(x, y) {
    // Optimization if they are referencially equal
    if (x === y) {
        return 0;
    } else {
        var keys = Object.getOwnPropertyNames(x);
        for (var i = 0; i < keys.length; i++) {
            var res = compare(x[keys[i]], y[keys[i]]);
            if (res !== 0) return res;
        }
        return 0;
    }
}
function equalsUnions(x, y) {
    // Optimization if they are referencially equal
    if (x === y) {
        return true;
    } else if (x.Case !== y.Case) {
        return false;
    } else {
        for (var i = 0; i < x.Fields.length; i++) {
            if (!equals(x.Fields[i], y.Fields[i])) return false;
        }
        return true;
    }
}
function compareUnions(x, y) {
    // Optimization if they are referencially equal
    if (x === y) {
        return 0;
    } else {
        var res = compare(x.Case, y.Case);
        if (res !== 0) return res;
        for (var i = 0; i < x.Fields.length; i++) {
            res = compare(x.Fields[i], y.Fields[i]);
            if (res !== 0) return res;
        }
        return 0;
    }
}

function createObj(fields) {
    var o = {};
    var _iteratorNormalCompletion = true;
    var _didIteratorError = false;
    var _iteratorError = undefined;

    try {
        for (var _iterator = fields[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
            var kv = _step.value;

            o[kv[0]] = kv[1];
        }
    } catch (err) {
        _didIteratorError = true;
        _iteratorError = err;
    } finally {
        try {
            if (!_iteratorNormalCompletion && _iterator.return) {
                _iterator.return();
            }
        } finally {
            if (_didIteratorError) {
                throw _iteratorError;
            }
        }
    }

    return o;
}

var _createClass$2 = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck$2(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

// This module is split from List.ts to prevent cyclic dependencies
function ofArray(args, base) {
    var acc = base || new List$1();
    for (var i = args.length - 1; i >= 0; i--) {
        acc = new List$1(args[i], acc);
    }
    return acc;
}

var List$1 = function () {
    function List(head, tail) {
        _classCallCheck$2(this, List);

        this.head = head;
        this.tail = tail;
    }

    _createClass$2(List, [{
        key: "ToString",
        value: function ToString() {
            return "[" + Array.from(this).map(toString).join("; ") + "]";
        }
    }, {
        key: "Equals",
        value: function Equals(x) {
            // Optimization if they are referencially equal
            if (this === x) {
                return true;
            } else {
                var iter1 = this[Symbol.iterator](),
                    iter2 = x[Symbol.iterator]();
                for (;;) {
                    var cur1 = iter1.next(),
                        cur2 = iter2.next();
                    if (cur1.done) return cur2.done ? true : false;else if (cur2.done) return false;else if (!equals(cur1.value, cur2.value)) return false;
                }
            }
        }
    }, {
        key: "CompareTo",
        value: function CompareTo(x) {
            // Optimization if they are referencially equal
            if (this === x) {
                return 0;
            } else {
                var acc = 0;
                var iter1 = this[Symbol.iterator](),
                    iter2 = x[Symbol.iterator]();
                for (;;) {
                    var cur1 = iter1.next(),
                        cur2 = iter2.next();
                    if (cur1.done) return cur2.done ? acc : -1;else if (cur2.done) return 1;else {
                        acc = compare(cur1.value, cur2.value);
                        if (acc != 0) return acc;
                    }
                }
            }
        }
    }, {
        key: Symbol.iterator,
        value: function value() {
            var cur = this;
            return {
                next: function next() {
                    var tmp = cur;
                    cur = cur.tail;
                    return { done: tmp.tail == null, value: tmp.head };
                }
            };
        }
        //   append(ys: List<T>): List<T> {
        //     return append(this, ys);
        //   }
        //   choose<U>(f: (x: T) => U, xs: List<T>): List<U> {
        //     return choose(f, this);
        //   }
        //   collect<U>(f: (x: T) => List<U>): List<U> {
        //     return collect(f, this);
        //   }
        //   filter(f: (x: T) => boolean): List<T> {
        //     return filter(f, this);
        //   }
        //   where(f: (x: T) => boolean): List<T> {
        //     return filter(f, this);
        //   }
        //   map<U>(f: (x: T) => U): List<U> {
        //     return map(f, this);
        //   }
        //   mapIndexed<U>(f: (i: number, x: T) => U): List<U> {
        //     return mapIndexed(f, this);
        //   }
        //   partition(f: (x: T) => boolean): [List<T>, List<T>] {
        //     return partition(f, this) as [List<T>, List<T>];
        //   }
        //   reverse(): List<T> {
        //     return reverse(this);
        //   }
        //   slice(lower: number, upper: number): List<T> {
        //     return slice(lower, upper, this);
        //   }

    }, {
        key: _Symbol.reflection,
        value: function value() {
            return {
                type: "Microsoft.FSharp.Collections.FSharpList",
                interfaces: ["System.IEquatable", "System.IComparable"]
            };
        }
    }, {
        key: "length",
        get: function get() {
            var cur = this,
                acc = 0;
            while (cur.tail != null) {
                cur = cur.tail;
                acc++;
            }
            return acc;
        }
    }]);

    return List;
}();

var _slicedToArray = function () { function sliceIterator(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"]) _i["return"](); } finally { if (_d) throw _e; } } return _arr; } return function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { return sliceIterator(arr, i); } else { throw new TypeError("Invalid attempt to destructure non-iterable instance"); } }; }();

function _defineProperty$1(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }










function compareWith(f, xs, ys) {
    var nonZero = tryFind(function (i) {
        return i != 0;
    }, map2(function (x, y) {
        return f(x, y);
    }, xs, ys));
    return nonZero != null ? nonZero : count(xs) - count(ys);
}
function delay(f) {
    return _defineProperty$1({}, Symbol.iterator, function () {
        return f()[Symbol.iterator]();
    });
}






function exists(f, xs) {
    function aux(iter) {
        var cur = iter.next();
        return !cur.done && (f(cur.value) || aux(iter));
    }
    return aux(xs[Symbol.iterator]());
}

function filter$1(f, xs) {
    function trySkipToNext(iter) {
        var cur = iter.next();
        while (!cur.done) {
            if (f(cur.value)) {
                return [cur.value, iter];
            }
            cur = iter.next();
        }
        return void 0;
    }
    return delay(function () {
        return unfold(trySkipToNext, xs[Symbol.iterator]());
    });
}

function fold(f, acc, xs) {
    if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
        return xs.reduce(f, acc);
    } else {
        var cur = void 0;
        for (var i = 0, iter = xs[Symbol.iterator]();; i++) {
            cur = iter.next();
            if (cur.done) break;
            acc = f(acc, cur.value, i);
        }
        return acc;
    }
}



function forAll(f, xs) {
    return fold(function (acc, x) {
        return acc && f(x);
    }, true, xs);
}







function iterate(f, xs) {
    fold(function (_, x) {
        return f(x);
    }, null, xs);
}






// A export function 'length' method causes problems in JavaScript -- https://github.com/Microsoft/TypeScript/issues/442
function count(xs) {
    return Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs.length : fold(function (acc, x) {
        return acc + 1;
    }, 0, xs);
}
function map$1(f, xs) {
    return delay(function () {
        return unfold(function (iter) {
            var cur = iter.next();
            return !cur.done ? [f(cur.value), iter] : null;
        }, xs[Symbol.iterator]());
    });
}

function map2(f, xs, ys) {
    return delay(function () {
        var iter1 = xs[Symbol.iterator]();
        var iter2 = ys[Symbol.iterator]();
        return unfold(function () {
            var cur1 = iter1.next(),
                cur2 = iter2.next();
            return !cur1.done && !cur2.done ? [f(cur1.value, cur2.value), null] : null;
        });
    });
}






























function tryFind(f, xs, defaultValue) {
    for (var i = 0, iter = xs[Symbol.iterator]();; i++) {
        var cur = iter.next();
        if (cur.done) return defaultValue === void 0 ? null : defaultValue;
        if (f(cur.value, i)) return cur.value;
    }
}









function unfold(f, acc) {
    return _defineProperty$1({}, Symbol.iterator, function () {
        return {
            next: function next() {
                var res = f(acc);
                if (res != null) {
                    acc = res[1];
                    return { done: false, value: res[0] };
                }
                return { done: true };
            }
        };
    });
}

var _createClass$4 = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck$4(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var GenericComparer = function () {
    function GenericComparer(f) {
        _classCallCheck$4(this, GenericComparer);

        this.Compare = f || compare;
    }

    _createClass$4(GenericComparer, [{
        key: _Symbol.reflection,
        value: function value() {
            return { interfaces: ["System.IComparer"] };
        }
    }]);

    return GenericComparer;
}();

var _createClass$3 = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck$3(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

// ----------------------------------------------
// These functions belong to Seq.ts but are
// implemented here to prevent cyclic dependencies


var MapTree = function MapTree(caseName, fields) {
    _classCallCheck$3(this, MapTree);

    this.Case = caseName;
    this.Fields = fields;
};
function tree_sizeAux(acc, m) {
    return m.Case === "MapOne" ? acc + 1 : m.Case === "MapNode" ? tree_sizeAux(tree_sizeAux(acc + 1, m.Fields[2]), m.Fields[3]) : acc;
}
function tree_size(x) {
    return tree_sizeAux(0, x);
}
function tree_empty() {
    return new MapTree("MapEmpty", []);
}
function tree_height(_arg1) {
    return _arg1.Case === "MapOne" ? 1 : _arg1.Case === "MapNode" ? _arg1.Fields[4] : 0;
}
function tree_mk(l, k, v, r) {
    var matchValue = [l, r];
    var $target1 = function $target1() {
        var hl = tree_height(l);
        var hr = tree_height(r);
        var m = hl < hr ? hr : hl;
        return new MapTree("MapNode", [k, v, l, r, m + 1]);
    };
    if (matchValue[0].Case === "MapEmpty") {
        if (matchValue[1].Case === "MapEmpty") {
            return new MapTree("MapOne", [k, v]);
        } else {
            return $target1();
        }
    } else {
        return $target1();
    }
}

function tree_rebalance(t1, k, v, t2) {
    var t1h = tree_height(t1);
    var t2h = tree_height(t2);
    if (t2h > t1h + 2) {
        if (t2.Case === "MapNode") {
            if (tree_height(t2.Fields[2]) > t1h + 1) {
                if (t2.Fields[2].Case === "MapNode") {
                    return tree_mk(tree_mk(t1, k, v, t2.Fields[2].Fields[2]), t2.Fields[2].Fields[0], t2.Fields[2].Fields[1], tree_mk(t2.Fields[2].Fields[3], t2.Fields[0], t2.Fields[1], t2.Fields[3]));
                } else {
                    throw new Error("rebalance");
                }
            } else {
                return tree_mk(tree_mk(t1, k, v, t2.Fields[2]), t2.Fields[0], t2.Fields[1], t2.Fields[3]);
            }
        } else {
            throw new Error("rebalance");
        }
    } else {
        if (t1h > t2h + 2) {
            if (t1.Case === "MapNode") {
                if (tree_height(t1.Fields[3]) > t2h + 1) {
                    if (t1.Fields[3].Case === "MapNode") {
                        return tree_mk(tree_mk(t1.Fields[2], t1.Fields[0], t1.Fields[1], t1.Fields[3].Fields[2]), t1.Fields[3].Fields[0], t1.Fields[3].Fields[1], tree_mk(t1.Fields[3].Fields[3], k, v, t2));
                    } else {
                        throw new Error("rebalance");
                    }
                } else {
                    return tree_mk(t1.Fields[2], t1.Fields[0], t1.Fields[1], tree_mk(t1.Fields[3], k, v, t2));
                }
            } else {
                throw new Error("rebalance");
            }
        } else {
            return tree_mk(t1, k, v, t2);
        }
    }
}
function tree_add(comparer, k, v, m) {
    if (m.Case === "MapOne") {
        var c = comparer.Compare(k, m.Fields[0]);
        if (c < 0) {
            return new MapTree("MapNode", [k, v, new MapTree("MapEmpty", []), m, 2]);
        } else if (c === 0) {
            return new MapTree("MapOne", [k, v]);
        }
        return new MapTree("MapNode", [k, v, m, new MapTree("MapEmpty", []), 2]);
    } else if (m.Case === "MapNode") {
        var c = comparer.Compare(k, m.Fields[0]);
        if (c < 0) {
            return tree_rebalance(tree_add(comparer, k, v, m.Fields[2]), m.Fields[0], m.Fields[1], m.Fields[3]);
        } else if (c === 0) {
            return new MapTree("MapNode", [k, v, m.Fields[2], m.Fields[3], m.Fields[4]]);
        }
        return tree_rebalance(m.Fields[2], m.Fields[0], m.Fields[1], tree_add(comparer, k, v, m.Fields[3]));
    }
    return new MapTree("MapOne", [k, v]);
}
function tree_find(comparer, k, m) {
    var res = tree_tryFind(comparer, k, m);
    if (res != null) return res;
    throw new Error("key not found");
}
function tree_tryFind(comparer, k, m) {
    if (m.Case === "MapOne") {
        var c = comparer.Compare(k, m.Fields[0]);
        return c === 0 ? m.Fields[1] : null;
    } else if (m.Case === "MapNode") {
        var c = comparer.Compare(k, m.Fields[0]);
        if (c < 0) {
            return tree_tryFind(comparer, k, m.Fields[2]);
        } else {
            if (c === 0) {
                return m.Fields[1];
            } else {
                return tree_tryFind(comparer, k, m.Fields[3]);
            }
        }
    }
    return null;
}
function tree_mem(comparer, k, m) {
    return m.Case === "MapOne" ? comparer.Compare(k, m.Fields[0]) === 0 : m.Case === "MapNode" ? function () {
        var c = comparer.Compare(k, m.Fields[0]);
        if (c < 0) {
            return tree_mem(comparer, k, m.Fields[2]);
        } else {
            if (c === 0) {
                return true;
            } else {
                return tree_mem(comparer, k, m.Fields[3]);
            }
        }
    }() : false;
}
// function tree_foldFromTo(comparer: IComparer<any>, lo: any, hi: any, f: (k:any, v:any, acc: any) => any, m: MapTree, x: any): any {
//   if (m.Case === "MapOne") {
//     var cLoKey = comparer.Compare(lo, m.Fields[0]);
//     var cKeyHi = comparer.Compare(m.Fields[0], hi);
//     var x_1 = (cLoKey <= 0 ? cKeyHi <= 0 : false) ? f(m.Fields[0], m.Fields[1], x) : x;
//     return x_1;
//   }
//   else if (m.Case === "MapNode") {
//     var cLoKey = comparer.Compare(lo, m.Fields[0]);
//     var cKeyHi = comparer.Compare(m.Fields[0], hi);
//     var x_1 = cLoKey < 0 ? tree_foldFromTo(comparer, lo, hi, f, m.Fields[2], x) : x;
//     var x_2 = (cLoKey <= 0 ? cKeyHi <= 0 : false) ? f(m.Fields[0], m.Fields[1], x_1) : x_1;
//     var x_3 = cKeyHi < 0 ? tree_foldFromTo(comparer, lo, hi, f, m.Fields[3], x_2) : x_2;
//     return x_3;
//   }
//   return x;
// }
// function tree_foldSection(comparer: IComparer<any>, lo: any, hi: any, f: (k:any, v:any, acc: any) => any, m: MapTree, x: any) {
//   return comparer.Compare(lo, hi) === 1 ? x : tree_foldFromTo(comparer, lo, hi, f, m, x);
// }
// function tree_loop(m: MapTree, acc: any): List<[any,any]> {
//   return m.Case === "MapOne"
//     ? new List([m.Fields[0], m.Fields[1]], acc)
//     : m.Case === "MapNode"
//       ? tree_loop(m.Fields[2], new List([m.Fields[0], m.Fields[1]], tree_loop(m.Fields[3], acc)))
//       : acc;
// }
// function tree_toList(m: MapTree) {
//   return tree_loop(m, new List());
// }
// function tree_toArray(m: MapTree) {
//   return Array.from(tree_toList(m));
// }
// function tree_ofList(comparer: IComparer<any>, l: List<[any,any]>) {
//   return Seq.fold((acc: MapTree, tupledArg: [any, any]) => {
//     return tree_add(comparer, tupledArg[0], tupledArg[1], acc);
//   }, tree_empty(), l);
// }
function tree_mkFromEnumerator(comparer, acc, e) {
    var cur = e.next();
    while (!cur.done) {
        acc = tree_add(comparer, cur.value[0], cur.value[1], acc);
        cur = e.next();
    }
    return acc;
}
// function tree_ofArray(comparer: IComparer<any>, arr: ArrayLike<[any,any]>) {
//   var res = tree_empty();
//   for (var i = 0; i <= arr.length - 1; i++) {
//     res = tree_add(comparer, arr[i][0], arr[i][1], res);
//   }
//   return res;
// }
function tree_ofSeq(comparer, c) {
    var ie = c[Symbol.iterator]();
    return tree_mkFromEnumerator(comparer, tree_empty(), ie);
}
// function tree_copyToArray(s: MapTree, arr: ArrayLike<any>, i: number) {
//   tree_iter((x, y) => { arr[i++] = [x, y]; }, s);
// }
function tree_collapseLHS(stack) {
    if (stack.tail != null) {
        if (stack.head.Case === "MapOne") {
            return stack;
        } else if (stack.head.Case === "MapNode") {
            return tree_collapseLHS(ofArray([stack.head.Fields[2], new MapTree("MapOne", [stack.head.Fields[0], stack.head.Fields[1]]), stack.head.Fields[3]], stack.tail));
        } else {
            return tree_collapseLHS(stack.tail);
        }
    } else {
        return new List$1();
    }
}
function tree_mkIterator(s) {
    return { stack: tree_collapseLHS(new List$1(s, new List$1())), started: false };
}
function tree_moveNext(i) {
    function current(i) {
        if (i.stack.tail == null) {
            return null;
        } else if (i.stack.head.Case === "MapOne") {
            return [i.stack.head.Fields[0], i.stack.head.Fields[1]];
        }
        throw new Error("Please report error: Map iterator, unexpected stack for current");
    }
    if (i.started) {
        if (i.stack.tail == null) {
            return { done: true, value: null };
        } else {
            if (i.stack.head.Case === "MapOne") {
                i.stack = tree_collapseLHS(i.stack.tail);
                return {
                    done: i.stack.tail == null,
                    value: current(i)
                };
            } else {
                throw new Error("Please report error: Map iterator, unexpected stack for moveNext");
            }
        }
    } else {
        i.started = true;
        return {
            done: i.stack.tail == null,
            value: current(i)
        };
    }
    
}

var FMap = function () {
    /** Do not call, use Map.create instead. */
    function FMap() {
        _classCallCheck$3(this, FMap);
    }

    _createClass$3(FMap, [{
        key: "ToString",
        value: function ToString() {
            return "map [" + Array.from(this).map(toString).join("; ") + "]";
        }
    }, {
        key: "Equals",
        value: function Equals(m2) {
            return this.CompareTo(m2) === 0;
        }
    }, {
        key: "CompareTo",
        value: function CompareTo(m2) {
            var _this = this;

            return this === m2 ? 0 : compareWith(function (kvp1, kvp2) {
                var c = _this.comparer.Compare(kvp1[0], kvp2[0]);
                return c !== 0 ? c : compare(kvp1[1], kvp2[1]);
            }, this, m2);
        }
    }, {
        key: Symbol.iterator,
        value: function value() {
            var i = tree_mkIterator(this.tree);
            return {
                next: function next() {
                    return tree_moveNext(i);
                }
            };
        }
    }, {
        key: "entries",
        value: function entries() {
            return this[Symbol.iterator]();
        }
    }, {
        key: "keys",
        value: function keys() {
            return map$1(function (kv) {
                return kv[0];
            }, this);
        }
    }, {
        key: "values",
        value: function values() {
            return map$1(function (kv) {
                return kv[1];
            }, this);
        }
    }, {
        key: "get",
        value: function get(k) {
            return tree_find(this.comparer, k, this.tree);
        }
    }, {
        key: "has",
        value: function has(k) {
            return tree_mem(this.comparer, k, this.tree);
        }
        /** Not supported */

    }, {
        key: "set",
        value: function set(k, v) {
            throw new Error("not supported");
        }
        /** Not supported */

    }, {
        key: "delete",
        value: function _delete(k) {
            throw new Error("not supported");
        }
        /** Not supported */

    }, {
        key: "clear",
        value: function clear() {
            throw new Error("not supported");
        }
    }, {
        key: _Symbol.reflection,
        value: function value() {
            return {
                type: "Microsoft.FSharp.Collections.FSharpMap",
                interfaces: ["System.IEquatable", "System.IComparable"]
            };
        }
    }, {
        key: "size",
        get: function get() {
            return tree_size(this.tree);
        }
    }]);

    return FMap;
}();

function from(comparer, tree) {
    var map$$1 = new FMap();
    map$$1.tree = tree;
    map$$1.comparer = comparer || new GenericComparer();
    return map$$1;
}
function create(ie, comparer) {
    comparer = comparer || new GenericComparer();
    return from(comparer, ie ? tree_ofSeq(comparer, ie) : tree_empty());
}
function add(k, v, map$$1) {
    return from(map$$1.comparer, tree_add(map$$1.comparer, k, v, map$$1.tree));
}





function tryFind$1(k, map$$1) {
    return tree_tryFind(map$$1.comparer, k, map$$1.tree);
}

function choose$$1(f, xs) {
    var r = fold(function (acc, x) {
        var y = f(x);
        return y != null ? new List$1(y, acc) : acc;
    }, new List$1(), xs);
    return reverse$$1(r);
}

// TODO: should be xs: Iterable<List<T>>

function filter$$1(f, xs) {
    return reverse$$1(fold(function (acc, x) {
        return f(x) ? new List$1(x, acc) : acc;
    }, new List$1(), xs));
}


function map$$1(f, xs) {
    return reverse$$1(fold(function (acc, x) {
        return new List$1(f(x), acc);
    }, new List$1(), xs));
}



function reverse$$1(xs) {
    return fold(function (acc, x) {
        return new List$1(x, acc);
    }, new List$1(), xs);
}


/* ToDo: instance unzip() */

/* ToDo: instance unzip3() */

var _createClass$5 = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck$5(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

// ----------------------------------------------
// These functions belong to Seq.ts but are
// implemented here to prevent cyclic dependencies


var SetTree = function SetTree(caseName, fields) {
    _classCallCheck$5(this, SetTree);

    this.Case = caseName;
    this.Fields = fields;
};
var tree_tolerance = 2;
function tree_countAux(s, acc) {
    return s.Case === "SetOne" ? acc + 1 : s.Case === "SetEmpty" ? acc : tree_countAux(s.Fields[1], tree_countAux(s.Fields[2], acc + 1));
}
function tree_count(s) {
    return tree_countAux(s, 0);
}
function tree_SetOne(n) {
    return new SetTree("SetOne", [n]);
}
function tree_SetNode(x, l, r, h) {
    return new SetTree("SetNode", [x, l, r, h]);
}
function tree_height$1(t) {
    return t.Case === "SetOne" ? 1 : t.Case === "SetNode" ? t.Fields[3] : 0;
}
function tree_mk$1(l, k, r) {
    var matchValue = [l, r];
    var $target1 = function $target1() {
        var hl = tree_height$1(l);
        var hr = tree_height$1(r);
        var m = hl < hr ? hr : hl;
        return tree_SetNode(k, l, r, m + 1);
    };
    if (matchValue[0].Case === "SetEmpty") {
        if (matchValue[1].Case === "SetEmpty") {
            return tree_SetOne(k);
        } else {
            return $target1();
        }
    } else {
        return $target1();
    }
}
function tree_rebalance$1(t1, k, t2) {
    var t1h = tree_height$1(t1);
    var t2h = tree_height$1(t2);
    if (t2h > t1h + tree_tolerance) {
        if (t2.Case === "SetNode") {
            if (tree_height$1(t2.Fields[1]) > t1h + 1) {
                if (t2.Fields[1].Case === "SetNode") {
                    return tree_mk$1(tree_mk$1(t1, k, t2.Fields[1].Fields[1]), t2.Fields[1].Fields[0], tree_mk$1(t2.Fields[1].Fields[2], t2.Fields[0], t2.Fields[2]));
                } else {
                    throw new Error("rebalance");
                }
            } else {
                return tree_mk$1(tree_mk$1(t1, k, t2.Fields[1]), t2.Fields[0], t2.Fields[2]);
            }
        } else {
            throw new Error("rebalance");
        }
    } else {
        if (t1h > t2h + tree_tolerance) {
            if (t1.Case === "SetNode") {
                if (tree_height$1(t1.Fields[2]) > t2h + 1) {
                    if (t1.Fields[2].Case === "SetNode") {
                        return tree_mk$1(tree_mk$1(t1.Fields[1], t1.Fields[0], t1.Fields[2].Fields[1]), t1.Fields[2].Fields[0], tree_mk$1(t1.Fields[2].Fields[2], k, t2));
                    } else {
                        throw new Error("rebalance");
                    }
                } else {
                    return tree_mk$1(t1.Fields[1], t1.Fields[0], tree_mk$1(t1.Fields[2], k, t2));
                }
            } else {
                throw new Error("rebalance");
            }
        } else {
            return tree_mk$1(t1, k, t2);
        }
    }
}
function tree_add$1(comparer, k, t) {
    return t.Case === "SetOne" ? function () {
        var c = comparer.Compare(k, t.Fields[0]);
        if (c < 0) {
            return tree_SetNode(k, new SetTree("SetEmpty", []), t, 2);
        } else {
            if (c === 0) {
                return t;
            } else {
                return tree_SetNode(k, t, new SetTree("SetEmpty", []), 2);
            }
        }
    }() : t.Case === "SetEmpty" ? tree_SetOne(k) : function () {
        var c = comparer.Compare(k, t.Fields[0]);
        if (c < 0) {
            return tree_rebalance$1(tree_add$1(comparer, k, t.Fields[1]), t.Fields[0], t.Fields[2]);
        } else {
            if (c === 0) {
                return t;
            } else {
                return tree_rebalance$1(t.Fields[1], t.Fields[0], tree_add$1(comparer, k, t.Fields[2]));
            }
        }
    }();
}
function tree_mem$1(comparer, k, t) {
    return t.Case === "SetOne" ? comparer.Compare(k, t.Fields[0]) === 0 : t.Case === "SetEmpty" ? false : function () {
        var c = comparer.Compare(k, t.Fields[0]);
        if (c < 0) {
            return tree_mem$1(comparer, k, t.Fields[1]);
        } else {
            if (c === 0) {
                return true;
            } else {
                return tree_mem$1(comparer, k, t.Fields[2]);
            }
        }
    }();
}
function tree_forall$1(f, m) {
    return m.Case === "SetOne" ? f(m.Fields[0]) : m.Case === "SetEmpty" ? true : (f(m.Fields[0]) ? tree_forall$1(f, m.Fields[1]) : false) ? tree_forall$1(f, m.Fields[2]) : false;
}
function tree_exists$1(f, m) {
    return m.Case === "SetOne" ? f(m.Fields[0]) : m.Case === "SetEmpty" ? false : (f(m.Fields[0]) ? true : tree_exists$1(f, m.Fields[1])) ? true : tree_exists$1(f, m.Fields[2]);
}
function tree_subset(comparer, a, b) {
    return tree_forall$1(function (x) {
        return tree_mem$1(comparer, x, b);
    }, a);
}
function tree_psubset(comparer, a, b) {
    return tree_forall$1(function (x) {
        return tree_mem$1(comparer, x, b);
    }, a) ? tree_exists$1(function (x) {
        return !tree_mem$1(comparer, x, a);
    }, b) : false;
}
function tree_collapseLHS$1(stack) {
    return stack.tail != null ? stack.head.Case === "SetOne" ? stack : stack.head.Case === "SetNode" ? tree_collapseLHS$1(ofArray([stack.head.Fields[1], tree_SetOne(stack.head.Fields[0]), stack.head.Fields[2]], stack.tail)) : tree_collapseLHS$1(stack.tail) : new List$1();
}
function tree_mkIterator$1(s) {
    return { stack: tree_collapseLHS$1(new List$1(s, new List$1())), started: false };
}

// function tree_notStarted() {
//   throw new Error("Enumeration not started");
// };
// var alreadyFinished = $exports.alreadyFinished = function () {
//   throw new Error("Enumeration already started");
// };
function tree_moveNext$1(i) {
    function current(i) {
        if (i.stack.tail == null) {
            return null;
        } else if (i.stack.head.Case === "SetOne") {
            return i.stack.head.Fields[0];
        }
        throw new Error("Please report error: Set iterator, unexpected stack for current");
    }
    if (i.started) {
        if (i.stack.tail == null) {
            return { done: true, value: null };
        } else {
            if (i.stack.head.Case === "SetOne") {
                i.stack = tree_collapseLHS$1(i.stack.tail);
                return {
                    done: i.stack.tail == null,
                    value: current(i)
                };
            } else {
                throw new Error("Please report error: Set iterator, unexpected stack for moveNext");
            }
        }
    } else {
        i.started = true;
        return {
            done: i.stack.tail == null,
            value: current(i)
        };
    }
    
}
function tree_compareStacks(comparer, l1, l2) {
    var $target8 = function $target8(n1k, t1) {
        return tree_compareStacks(comparer, ofArray([new SetTree("SetEmpty", []), tree_SetOne(n1k)], t1), l2);
    };
    var $target9 = function $target9(n1k, n1l, n1r, t1) {
        return tree_compareStacks(comparer, ofArray([n1l, tree_SetNode(n1k, new SetTree("SetEmpty", []), n1r, 0)], t1), l2);
    };
    var $target11 = function $target11(n2k, n2l, n2r, t2) {
        return tree_compareStacks(comparer, l1, ofArray([n2l, tree_SetNode(n2k, new SetTree("SetEmpty", []), n2r, 0)], t2));
    };
    if (l1.tail != null) {
        if (l2.tail != null) {
            if (l2.head.Case === "SetOne") {
                if (l1.head.Case === "SetOne") {
                    var n1k = l1.head.Fields[0],
                        n2k = l2.head.Fields[0],
                        t1 = l1.tail,
                        t2 = l2.tail,
                        c = comparer.Compare(n1k, n2k);
                    if (c !== 0) {
                        return c;
                    } else {
                        return tree_compareStacks(comparer, t1, t2);
                    }
                } else {
                    if (l1.head.Case === "SetNode") {
                        if (l1.head.Fields[1].Case === "SetEmpty") {
                            var emp = l1.head.Fields[1],
                                _n1k = l1.head.Fields[0],
                                n1r = l1.head.Fields[2],
                                _n2k = l2.head.Fields[0],
                                _t = l1.tail,
                                _t2 = l2.tail,
                                _c = comparer.Compare(_n1k, _n2k);
                            if (_c !== 0) {
                                return _c;
                            } else {
                                return tree_compareStacks(comparer, ofArray([n1r], _t), ofArray([emp], _t2));
                            }
                        } else {
                            return $target9(l1.head.Fields[0], l1.head.Fields[1], l1.head.Fields[2], l1.tail);
                        }
                    } else {
                        var _n2k2 = l2.head.Fields[0],
                            _t3 = l2.tail;
                        return tree_compareStacks(comparer, l1, ofArray([new SetTree("SetEmpty", []), tree_SetOne(_n2k2)], _t3));
                    }
                }
            } else {
                if (l2.head.Case === "SetNode") {
                    if (l2.head.Fields[1].Case === "SetEmpty") {
                        if (l1.head.Case === "SetOne") {
                            var _n1k2 = l1.head.Fields[0],
                                _n2k3 = l2.head.Fields[0],
                                n2r = l2.head.Fields[2],
                                _t4 = l1.tail,
                                _t5 = l2.tail,
                                _c2 = comparer.Compare(_n1k2, _n2k3);
                            if (_c2 !== 0) {
                                return _c2;
                            } else {
                                return tree_compareStacks(comparer, ofArray([new SetTree("SetEmpty", [])], _t4), ofArray([n2r], _t5));
                            }
                        } else {
                            if (l1.head.Case === "SetNode") {
                                if (l1.head.Fields[1].Case === "SetEmpty") {
                                    var _n1k3 = l1.head.Fields[0],
                                        _n1r = l1.head.Fields[2],
                                        _n2k4 = l2.head.Fields[0],
                                        _n2r = l2.head.Fields[2],
                                        _t6 = l1.tail,
                                        _t7 = l2.tail,
                                        _c3 = comparer.Compare(_n1k3, _n2k4);
                                    if (_c3 !== 0) {
                                        return _c3;
                                    } else {
                                        return tree_compareStacks(comparer, ofArray([_n1r], _t6), ofArray([_n2r], _t7));
                                    }
                                } else {
                                    return $target9(l1.head.Fields[0], l1.head.Fields[1], l1.head.Fields[2], l1.tail);
                                }
                            } else {
                                return $target11(l2.head.Fields[0], l2.head.Fields[1], l2.head.Fields[2], l2.tail);
                            }
                        }
                    } else {
                        if (l1.head.Case === "SetOne") {
                            return $target8(l1.head.Fields[0], l1.tail);
                        } else {
                            if (l1.head.Case === "SetNode") {
                                return $target9(l1.head.Fields[0], l1.head.Fields[1], l1.head.Fields[2], l1.tail);
                            } else {
                                return $target11(l2.head.Fields[0], l2.head.Fields[1], l2.head.Fields[2], l2.tail);
                            }
                        }
                    }
                } else {
                    if (l1.head.Case === "SetOne") {
                        return $target8(l1.head.Fields[0], l1.tail);
                    } else {
                        if (l1.head.Case === "SetNode") {
                            return $target9(l1.head.Fields[0], l1.head.Fields[1], l1.head.Fields[2], l1.tail);
                        } else {
                            return tree_compareStacks(comparer, l1.tail, l2.tail);
                        }
                    }
                }
            }
        } else {
            return 1;
        }
    } else {
        if (l2.tail != null) {
            return -1;
        } else {
            return 0;
        }
    }
}
function tree_compare(comparer, s1, s2) {
    if (s1.Case === "SetEmpty") {
        if (s2.Case === "SetEmpty") {
            return 0;
        } else {
            return -1;
        }
    } else {
        if (s2.Case === "SetEmpty") {
            return 1;
        } else {
            return tree_compareStacks(comparer, ofArray([s1]), ofArray([s2]));
        }
    }
}
function tree_mkFromEnumerator$1(comparer, acc, e) {
    var cur = e.next();
    while (!cur.done) {
        acc = tree_add$1(comparer, cur.value, acc);
        cur = e.next();
    }
    return acc;
}
function tree_ofSeq$1(comparer, c) {
    var ie = c[Symbol.iterator]();
    return tree_mkFromEnumerator$1(comparer, new SetTree("SetEmpty", []), ie);
}

var FSet = function () {
    /** Do not call, use Set.create instead. */
    function FSet() {
        _classCallCheck$5(this, FSet);
    }

    _createClass$5(FSet, [{
        key: "ToString",
        value: function ToString() {
            return "set [" + Array.from(this).map(toString).join("; ") + "]";
        }
    }, {
        key: "Equals",
        value: function Equals(s2) {
            return this.CompareTo(s2) === 0;
        }
    }, {
        key: "CompareTo",
        value: function CompareTo(s2) {
            return this === s2 ? 0 : tree_compare(this.comparer, this.tree, s2.tree);
        }
    }, {
        key: Symbol.iterator,
        value: function value() {
            var i = tree_mkIterator$1(this.tree);
            return {
                next: function next() {
                    return tree_moveNext$1(i);
                }
            };
        }
    }, {
        key: "values",
        value: function values() {
            return this[Symbol.iterator]();
        }
    }, {
        key: "has",
        value: function has(v) {
            return tree_mem$1(this.comparer, v, this.tree);
        }
        /** Not supported */

    }, {
        key: "add",
        value: function add$1(v) {
            throw new Error("not supported");
        }
        /** Not supported */

    }, {
        key: "delete",
        value: function _delete(v) {
            throw new Error("not supported");
        }
        /** Not supported */

    }, {
        key: "clear",
        value: function clear() {
            throw new Error("not supported");
        }
    }, {
        key: _Symbol.reflection,
        value: function value() {
            return {
                type: "Microsoft.FSharp.Collections.FSharpSet",
                interfaces: ["System.IEquatable", "System.IComparable"]
            };
        }
    }, {
        key: "size",
        get: function get() {
            return tree_count(this.tree);
        }
    }]);

    return FSet;
}();

function from$1(comparer, tree) {
    var s = new FSet();
    s.tree = tree;
    s.comparer = comparer || new GenericComparer();
    return s;
}
function create$1(ie, comparer) {
    comparer = comparer || new GenericComparer();
    return from$1(comparer, ie ? tree_ofSeq$1(comparer, ie) : new SetTree("SetEmpty", []));
}

function add$1(item$$1, s) {
    return from$1(s.comparer, tree_add$1(s.comparer, item$$1, s.tree));
}

function resolveGeneric(idx, enclosing) {
    try {
        var t = enclosing.head;
        if (t instanceof NonDeclaredType) {
            return resolveGeneric(idx, enclosing.tail);
        } else {
            var generics = t.prototype[_Symbol.generics]();
            var name = typeof idx === "string" ? idx : Object.getOwnPropertyNames(generics)[idx];
            var resolved = generics[name];
            return resolved instanceof NonDeclaredType && resolved.kind === "GenericParam" ? resolveGeneric(resolved.name, enclosing.tail) : new List$1(resolved, enclosing);
        }
    } catch (err) {
        throw new Error("Cannot resolve generic argument " + idx + ": " + err);
    }
}

// TODO: This needs improvement, check namespace for non-custom types?

function fromTicks(ticks) {
    return ticks / 10000;
}

function __getValue(d, key) {
    return d[(d.kind == 1 /* UTC */ ? "getUTC" : "get") + key]();
}


function parse$1(v, kind) {
    var date = v == null ? new Date() : new Date(v);
    if (isNaN(date.getTime())) throw new Error("The string is not a valid Date.");
    date.kind = kind || (typeof v == "string" && v.slice(-1) == "Z" ? 1 /* UTC */ : 2 /* Local */);
    return date;
}

function create$2(year, month, day) /* Local */{
    var h = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 0;
    var m = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : 0;
    var s = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : 0;
    var ms = arguments.length > 6 && arguments[6] !== undefined ? arguments[6] : 0;
    var kind = arguments.length > 7 && arguments[7] !== undefined ? arguments[7] : 2;

    var date = kind === 1 /* UTC */ ? new Date(Date.UTC(year, month - 1, day, h, m, s, ms)) : new Date(year, month - 1, day, h, m, s, ms);
    if (isNaN(date.getTime())) throw new Error("The parameters describe an unrepresentable Date.");
    date.kind = kind;
    return date;
}



function isLeapYear(year) {
    return year % 4 == 0 && year % 100 != 0 || year % 400 == 0;
}
function daysInMonth(year, month) {
    return month == 2 ? isLeapYear(year) ? 29 : 28 : month >= 8 ? month % 2 == 0 ? 31 : 30 : month % 2 == 0 ? 30 : 31;
}




function day(d) {
    return __getValue(d, "Date");
}
function hour(d) {
    return __getValue(d, "Hours");
}
function millisecond(d) {
    return __getValue(d, "Milliseconds");
}
function minute(d) {
    return __getValue(d, "Minutes");
}
function month(d) {
    return __getValue(d, "Month") + 1;
}
function second(d) {
    return __getValue(d, "Seconds");
}
function year(d) {
    return __getValue(d, "FullYear");
}

var _typeof$1 = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; };

function _defineProperty$2(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

function toJson(o) {
    return JSON.stringify(o, function (k, v) {
        if (ArrayBuffer.isView(v)) {
            return Array.from(v);
        } else if (v != null && (typeof v === "undefined" ? "undefined" : _typeof$1(v)) === "object") {
            var properties = typeof v[_Symbol.reflection] === "function" ? v[_Symbol.reflection]().properties : null;
            if (v instanceof List$1 || v instanceof FSet || v instanceof Set) {
                return Array.from(v);
            } else if (v instanceof FMap || v instanceof Map) {
                return fold(function (o, kv) {
                    return o[toJson(kv[0])] = kv[1], o;
                }, {}, v);
            } else if (!hasInterface(v, "FSharpRecord") && properties) {
                return fold(function (o, prop) {
                    return o[prop] = v[prop], o;
                }, {}, Object.getOwnPropertyNames(properties));
            } else if (hasInterface(v, "FSharpUnion")) {
                if (!v.Fields || !v.Fields.length) {
                    return v.Case;
                } else if (v.Fields.length === 1) {
                    return _defineProperty$2({}, v.Case, v.Fields[0]);
                } else {
                    return _defineProperty$2({}, v.Case, v.Fields);
                }
            }
        }
        return v;
    });
}
function inflate(val, typ) {
    function needsInflate(enclosing) {
        var typ = enclosing.head;
        if (typeof typ === "string") {
            return false;
        }
        if (typ instanceof NonDeclaredType) {
            switch (typ.kind) {
                case "Option":
                case "Array":
                    return needsInflate(new List$1(typ.generics[0], enclosing));
                case "Tuple":
                    return typ.generics.some(function (x) {
                        return needsInflate(new List$1(x, enclosing));
                    });
                case "GenericParam":
                    return needsInflate(resolveGeneric(typ.name, enclosing.tail));
                default:
                    return false;
            }
        }
        return true;
    }
    function inflateArray(arr, enclosing) {
        return Array.isArray(arr) && needsInflate(enclosing) ? arr.map(function (x) {
            return inflate(x, enclosing);
        }) : arr;
    }
    function inflateMap(obj, keyEnclosing, valEnclosing) {
        var inflateKey = keyEnclosing.head !== "string";
        var inflateVal = needsInflate(valEnclosing);
        return Object.getOwnPropertyNames(obj).map(function (k) {
            var key = inflateKey ? inflate(JSON.parse(k), keyEnclosing) : k;
            var val = inflateVal ? inflate(obj[k], valEnclosing) : obj[k];
            return [key, val];
        });
    }
    var enclosing = null;
    if (typ instanceof List$1) {
        enclosing = typ;
        typ = typ.head;
    } else {
        enclosing = new List$1(typ, new List$1());
    }
    if (val == null || typeof typ === "string") {
        return val;
    } else if (typ instanceof NonDeclaredType) {
        switch (typ.kind) {
            case "Unit":
                return null;
            case "Option":
                return inflate(val, new List$1(typ.generics[0], enclosing));
            case "Array":
                return inflateArray(val, new List$1(typ.generics[0], enclosing));
            case "Tuple":
                return typ.generics.map(function (x, i) {
                    return inflate(val[i], new List$1(x, enclosing));
                });
            case "GenericParam":
                return inflate(val, resolveGeneric(typ.name, enclosing.tail));
            // case "Interface": // case "Any":
            default:
                return val;
        }
    } else if (typeof typ === "function") {
        var proto = typ.prototype;
        if (typ === Date) {
            return parse$1(val);
        }
        if (proto instanceof List$1) {
            return ofArray(inflateArray(val, resolveGeneric(0, enclosing)));
        }
        if (proto instanceof FSet) {
            return create$1(inflateArray(val, resolveGeneric(0, enclosing)));
        }
        if (proto instanceof Set) {
            return new Set(inflateArray(val, resolveGeneric(0, enclosing)));
        }
        if (proto instanceof FMap) {
            return create(inflateMap(val, resolveGeneric(0, enclosing), resolveGeneric(1, enclosing)));
        }
        if (proto instanceof Map) {
            return new Map(inflateMap(val, resolveGeneric(0, enclosing), resolveGeneric(1, enclosing)));
        }
        // Union types
        var info = typeof proto[_Symbol.reflection] === "function" ? proto[_Symbol.reflection]() : {};
        if (info.cases) {
            var u = { Fields: [] };
            if (typeof val === "string") {
                u.Case = val;
            } else {
                var caseName = Object.getOwnPropertyNames(val)[0];
                var fieldTypes = info.cases[caseName];
                var fields = fieldTypes.length > 1 ? val[caseName] : [val[caseName]];
                u.Case = caseName;
                for (var i = 0; i < fieldTypes.length; i++) {
                    u.Fields.push(inflate(fields[i], new List$1(fieldTypes[i], enclosing)));
                }
            }
            return Object.assign(new typ(), u);
        }
        if (info.properties) {
            var properties = info.properties;
            var _iteratorNormalCompletion = true;
            var _didIteratorError = false;
            var _iteratorError = undefined;

            try {
                for (var _iterator = Object.getOwnPropertyNames(properties)[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
                    var k = _step.value;

                    val[k] = inflate(val[k], new List$1(properties[k], enclosing));
                }
            } catch (err) {
                _didIteratorError = true;
                _iteratorError = err;
            } finally {
                try {
                    if (!_iteratorNormalCompletion && _iterator.return) {
                        _iterator.return();
                    }
                } finally {
                    if (_didIteratorError) {
                        throw _iteratorError;
                    }
                }
            }

            return Object.assign(new typ(), val);
        }
        return val;
    }
    throw new Error("Unexpected type when deserializing JSON: " + typ);
}
function ofJson(json, genArgs) {
    return inflate(JSON.parse(json), genArgs ? genArgs.T : null);
}

function create$4(pattern, options) {
    var flags = "g";
    flags += options & 1 ? "i" : "";
    flags += options & 2 ? "m" : "";
    return new RegExp(pattern, flags);
}
// From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex
function escape(str) {
    return str.replace(/[\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&");
}



function matches(str, pattern) {
    var options = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 0;

    var reg = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options, reg) : reg = create$4(pattern, options);
    if (!reg.global) throw new Error("Non-global RegExp"); // Prevent infinite loop
    var m = void 0;
    var matches = [];
    while ((m = reg.exec(str)) !== null) {
        matches.push(m);
    }return matches;
}

var _typeof$3 = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; };

var fsFormatRegExp = /(^|[^%])%([0+ ]*)(-?\d+)?(?:\.(\d+))?(\w)/;
var formatRegExp = /\{(\d+)(,-?\d+)?(?:\:(.+?))?\}/g;
function fsFormat(str) {
    var _cont = void 0;
    function isObject(x) {
        return x !== null && (typeof x === "undefined" ? "undefined" : _typeof$3(x)) === "object" && !(x instanceof Number) && !(x instanceof String) && !(x instanceof Boolean);
    }
    function formatOnce(str, rep) {
        return str.replace(fsFormatRegExp, function (_, prefix, flags, pad, precision, format) {
            switch (format) {
                case "f":
                case "F":
                    rep = rep.toFixed(precision || 6);
                    break;
                case "g":
                case "G":
                    rep = rep.toPrecision(precision);
                    break;
                case "e":
                case "E":
                    rep = rep.toExponential(precision);
                    break;
                case "O":
                    rep = toString(rep);
                    break;
                case "A":
                    try {
                        rep = JSON.stringify(rep, function (k, v) {
                            return v && v[Symbol.iterator] && !Array.isArray(v) && isObject(v) ? Array.from(v) : v;
                        });
                    } catch (err) {
                        // Fallback for objects with circular references
                        rep = "{" + Object.getOwnPropertyNames(rep).map(function (k) {
                            return k + ": " + String(rep[k]);
                        }).join(", ") + "}";
                    }
                    break;
            }
            var plusPrefix = flags.indexOf("+") >= 0 && parseInt(rep) >= 0;
            if (!isNaN(pad = parseInt(pad))) {
                var ch = pad >= 0 && flags.indexOf("0") >= 0 ? "0" : " ";
                rep = padLeft(rep, Math.abs(pad) - (plusPrefix ? 1 : 0), ch, pad < 0);
            }
            var once = prefix + (plusPrefix ? "+" + rep : rep);
            return once.replace(/%/g, "%%");
        });
    }
    function makeFn(str) {
        return function (rep) {
            var str2 = formatOnce(str, rep);
            return fsFormatRegExp.test(str2) ? makeFn(str2) : _cont(str2.replace(/%%/g, "%"));
        };
    }

    for (var _len = arguments.length, args = Array(_len > 1 ? _len - 1 : 0), _key = 1; _key < _len; _key++) {
        args[_key - 1] = arguments[_key];
    }

    if (args.length === 0) {
        return function (cont) {
            _cont = cont;
            return fsFormatRegExp.test(str) ? makeFn(str) : _cont(str);
        };
    } else {
        for (var i = 0; i < args.length; i++) {
            str = formatOnce(str, args[i]);
        }
        return str.replace(/%%/g, "%");
    }
}




function isNullOrEmpty(str) {
    return typeof str !== "string" || str.length == 0;
}

function join(delimiter, xs) {
    xs = typeof xs == "string" ? getRestParams(arguments, 1) : xs;
    return (Array.isArray(xs) ? xs : Array.from(xs)).join(delimiter);
}

function padLeft(str, len, ch, isRight) {
    ch = ch || " ";
    str = String(str);
    len = len - str.length;
    for (var i = -1; ++i < len;) {
        str = isRight ? str + ch : ch + str;
    }return str;
}


function replace$$1(str, search, replace$$1) {
    return str.replace(new RegExp(escape(search), "g"), replace$$1);
}

var _createClass$7 = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck$7(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var Result = function () {
    function Result(caseName, fields) {
        _classCallCheck$7(this, Result);

        this.Case = caseName;
        this.Fields = fields;
    }

    _createClass$7(Result, [{
        key: _Symbol.reflection,
        value: function value() {
            return {
                type: "Fable.PowerPack.Result.Result",
                interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
                cases: {
                    Error: [GenericParam("B")],
                    Ok: [GenericParam("A")]
                }
            };
        }
    }, {
        key: "Equals",
        value: function Equals(other) {
            return equalsUnions(this, other);
        }
    }, {
        key: "CompareTo",
        value: function CompareTo(other) {
            return compareUnions(this, other);
        }
    }]);

    return Result;
}();
setType("Fable.PowerPack.Result.Result", Result);

function map$4(fn, a) {
    return a.Case === "Error" ? new Result("Error", [a.Fields[0]]) : new Result("Ok", [fn(a.Fields[0])]);
}
function bind(fn, a) {
    return a.Case === "Error" ? new Result("Error", [a.Fields[0]]) : fn(a.Fields[0]);
}
var ResultBuilder = function () {
    _createClass$7(ResultBuilder, [{
        key: _Symbol.reflection,
        value: function value() {
            return {
                type: "Fable.PowerPack.Result.ResultBuilder",
                properties: {}
            };
        }
    }]);

    function ResultBuilder() {
        _classCallCheck$7(this, ResultBuilder);
    }

    _createClass$7(ResultBuilder, [{
        key: "Bind",
        value: function Bind(m, f) {
            return bind(f, m);
        }
    }, {
        key: "Return",
        value: function Return(a) {
            return new Result("Ok", [a]);
        }
    }, {
        key: "ReturnFrom",
        value: function ReturnFrom(m) {
            return m;
        }
    }, {
        key: "Combine",
        value: function Combine(left, right) {
            return this.Bind(left, function () {
                return right;
            });
        }
    }, {
        key: "Zero",
        get: function get() {
            var _this = this;

            return function (arg00) {
                return _this.Return(arg00);
            };
        }
    }]);

    return ResultBuilder;
}();
setType("Fable.PowerPack.Result.ResultBuilder", ResultBuilder);
var result = new ResultBuilder();

var _createClass$6 = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck$6(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var _Promise = function (__exports) {
    var result$$1 = __exports.result = function result$$1(a) {
        return a.then(function (arg0) {
            return new Result("Ok", [arg0]);
        }, function (arg0) {
            return new Result("Error", [arg0]);
        });
    };

    var mapResult = __exports.mapResult = function mapResult(fn, a) {
        return a.then(function (a_1) {
            return map$4(fn, a_1);
        });
    };

    var bindResult = __exports.bindResult = function bindResult(fn, a) {
        return a.then(function (a_1) {
            return a_1.Case === "Error" ? Promise.resolve(new Result("Error", [a_1.Fields[0]])) : result$$1(fn(a_1.Fields[0]));
        });
    };

    var PromiseBuilder = __exports.PromiseBuilder = function () {
        _createClass$6(PromiseBuilder, [{
            key: _Symbol.reflection,
            value: function value() {
                return {
                    type: "Fable.PowerPack.Promise.PromiseBuilder",
                    properties: {}
                };
            }
        }]);

        function PromiseBuilder() {
            _classCallCheck$6(this, PromiseBuilder);
        }

        _createClass$6(PromiseBuilder, [{
            key: "For",
            value: function For(seq, body) {
                var p = Promise.resolve();
                var _iteratorNormalCompletion = true;
                var _didIteratorError = false;
                var _iteratorError = undefined;

                try {
                    var _loop = function _loop() {
                        var a = _step.value;
                        p = p.then(function () {
                            return body(a);
                        });
                    };

                    for (var _iterator = seq[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
                        _loop();
                    }
                } catch (err) {
                    _didIteratorError = true;
                    _iteratorError = err;
                } finally {
                    try {
                        if (!_iteratorNormalCompletion && _iterator.return) {
                            _iterator.return();
                        }
                    } finally {
                        if (_didIteratorError) {
                            throw _iteratorError;
                        }
                    }
                }

                return p;
            }
        }, {
            key: "While",
            value: function While(guard, p) {
                var _this = this;

                return guard() ? p.then(function () {
                    return _this.While(guard, p);
                }) : Promise.resolve();
            }
        }, {
            key: "TryFinally",
            value: function TryFinally(p, compensation) {
                return p.then(function (x) {
                    compensation();
                    return x;
                }, function (er) {
                    compensation();
                    throw er;
                });
            }
        }, {
            key: "Delay",
            value: function Delay(generator) {
                return {
                    then: function then(f1, f2) {
                        try {
                            return generator().then(f1, f2);
                        } catch (er) {
                            if (f2 == null) {
                                return Promise.reject(er);
                            } else {
                                try {
                                    return Promise.resolve(f2(er));
                                } catch (er_1) {
                                    return Promise.reject(er_1);
                                }
                            }
                        }
                    },
                    catch: function _catch(f) {
                        try {
                            return generator().catch(f);
                        } catch (er) {
                            try {
                                return Promise.resolve(f(er));
                            } catch (er_1) {
                                return Promise.reject(er_1);
                            }
                        }
                    }
                };
            }
        }, {
            key: "Using",
            value: function Using(resource, binder) {
                return this.TryFinally(binder(resource), function () {
                    resource.Dispose();
                });
            }
        }]);

        return PromiseBuilder;
    }();

    setType("Fable.PowerPack.Promise.PromiseBuilder", PromiseBuilder);
    return __exports;
}({});

var PromiseImpl = function (__exports) {
    var promise = __exports.promise = new _Promise.PromiseBuilder();
    return __exports;
}({});

function _fetch(url, init) {
    return fetch(url, init).then(function (response) {
        return response.ok ? response : function () {
            throw new Error(String(response.status) + " " + response.statusText + " for URL " + response.url);
        }();
    });
}

var _createClass$8 = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck$8(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var Types = function (__exports) {
    var Attribute = __exports.Attribute = function () {
        function Attribute(caseName, fields) {
            _classCallCheck$8(this, Attribute);

            this.Case = caseName;
            this.Fields = fields;
        }

        _createClass$8(Attribute, [{
            key: _Symbol.reflection,
            value: function () {
                return {
                    type: "Fable.Arch.Html.Types.Attribute",
                    interfaces: ["FSharpUnion"],
                    cases: {
                        Attribute: [Tuple(["string", "string"])],
                        EventHandler: [Tuple(["string", "function"])],
                        Property: [Tuple(["string", "string"])],
                        Style: [makeGeneric(List$1, {
                            T: Tuple(["string", "string"])
                        })]
                    }
                };
            }
        }]);

        return Attribute;
    }();

    setType("Fable.Arch.Html.Types.Attribute", Attribute);

    var DomNode = __exports.DomNode = function () {
        function DomNode(caseName, fields) {
            _classCallCheck$8(this, DomNode);

            this.Case = caseName;
            this.Fields = fields;
        }

        _createClass$8(DomNode, [{
            key: _Symbol.reflection,
            value: function () {
                return {
                    type: "Fable.Arch.Html.Types.DomNode",
                    interfaces: ["FSharpUnion"],
                    cases: {
                        Element: [Tuple(["string", makeGeneric(List$1, {
                            T: makeGeneric(Attribute, {
                                TMessage: GenericParam("TMessage")
                            })
                        })]), makeGeneric(List$1, {
                            T: makeGeneric(DomNode, {
                                TMessage: GenericParam("TMessage")
                            })
                        })],
                        Svg: [Tuple(["string", makeGeneric(List$1, {
                            T: makeGeneric(Attribute, {
                                TMessage: GenericParam("TMessage")
                            })
                        })]), makeGeneric(List$1, {
                            T: makeGeneric(DomNode, {
                                TMessage: GenericParam("TMessage")
                            })
                        })],
                        Text: ["string"],
                        VoidElement: [Tuple(["string", makeGeneric(List$1, {
                            T: makeGeneric(Attribute, {
                                TMessage: GenericParam("TMessage")
                            })
                        })])],
                        WhiteSpace: ["string"]
                    }
                };
            }
        }]);

        return DomNode;
    }();

    setType("Fable.Arch.Html.Types.DomNode", DomNode);
    return __exports;
}({});
function mapEventHandler(mapping, e, f) {
    return new Types.Attribute("EventHandler", [[e, function ($var1) {
        return mapping(f($var1));
    }]]);
}
function mapAttributes(mapping, attribute) {
    return attribute.Case === "Style" ? new Types.Attribute("Style", [attribute.Fields[0]]) : attribute.Case === "Property" ? new Types.Attribute("Property", [attribute.Fields[0]]) : attribute.Case === "Attribute" ? new Types.Attribute("Attribute", [attribute.Fields[0]]) : mapEventHandler(mapping, attribute.Fields[0][0], attribute.Fields[0][1]);
}
function mapElem(mapping, node_0, node_1) {
    var node = [node_0, node_1];
    return [node[0], map$$1(function (attribute) {
        return mapAttributes(mapping, attribute);
    }, node[1])];
}
function mapVoidElem(mapping, node_0, node_1) {
    var node = [node_0, node_1];
    return [node[0], map$$1(function (attribute) {
        return mapAttributes(mapping, attribute);
    }, node[1])];
}
function map$5(mapping, node) {
    return node.Case === "VoidElement" ? new Types.DomNode("VoidElement", [mapVoidElem(mapping, node.Fields[0][0], node.Fields[0][1])]) : node.Case === "Text" ? new Types.DomNode("Text", [node.Fields[0]]) : node.Case === "WhiteSpace" ? new Types.DomNode("WhiteSpace", [node.Fields[0]]) : node.Case === "Svg" ? new Types.DomNode("Element", [mapElem(mapping, node.Fields[0][0], node.Fields[0][1]), map$$1(function (node_1) {
        return map$5(mapping, node_1);
    }, node.Fields[1])]) : new Types.DomNode("Element", [mapElem(mapping, node.Fields[0][0], node.Fields[0][1]), map$$1(function (node_1) {
        return map$5(mapping, node_1);
    }, node.Fields[1])]);
}
var Attributes = function (__exports) {
    var classBaseList = __exports.classBaseList = function (b, list) {
        return new Types.Attribute("Attribute", [["class", fsFormat("%s %s")(function (x) {
            return x;
        })(b)(join(" ", map$1(function (tupledArg) {
            return tupledArg[0];
        }, filter$1(function (tupledArg) {
            return tupledArg[1];
        }, list))))]]);
    };

    return __exports;
}({});
var Svg = function (__exports) {
    var svgNS = __exports.svgNS = function () {
        return new Types.Attribute("Property", [["namespace", "http://www.w3.org/2000/svg"]]);
    };

    return __exports;
}({});

var _createClass$9 = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck$9(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var Types$1 = function (__exports) {
    var ModelChanged = __exports.ModelChanged = function () {
        function ModelChanged(previousState, message, currentState) {
            _classCallCheck$9(this, ModelChanged);

            this.PreviousState = previousState;
            this.Message = message;
            this.CurrentState = currentState;
        }

        _createClass$9(ModelChanged, [{
            key: _Symbol.reflection,
            value: function () {
                return {
                    type: "Fable.Arch.App.Types.ModelChanged",
                    interfaces: ["FSharpRecord", "System.IEquatable", "System.IComparable"],
                    properties: {
                        PreviousState: GenericParam("TModel"),
                        Message: GenericParam("TMessage"),
                        CurrentState: GenericParam("TModel")
                    }
                };
            }
        }, {
            key: "Equals",
            value: function (other) {
                return equalsRecords(this, other);
            }
        }, {
            key: "CompareTo",
            value: function (other) {
                return compareRecords(this, other);
            }
        }]);

        return ModelChanged;
    }();

    setType("Fable.Arch.App.Types.ModelChanged", ModelChanged);

    var AppEvent = __exports.AppEvent = function () {
        function AppEvent(caseName, fields) {
            _classCallCheck$9(this, AppEvent);

            this.Case = caseName;
            this.Fields = fields;
        }

        _createClass$9(AppEvent, [{
            key: _Symbol.reflection,
            value: function () {
                return {
                    type: "Fable.Arch.App.Types.AppEvent",
                    interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
                    cases: {
                        ActionReceived: [GenericParam("TMessage")],
                        ModelChanged: [makeGeneric(ModelChanged, {
                            TMessage: GenericParam("TMessage"),
                            TModel: GenericParam("TModel")
                        })],
                        Replayed: [makeGeneric(List$1, {
                            T: Tuple(["string", GenericParam("TModel")])
                        })]
                    }
                };
            }
        }, {
            key: "Equals",
            value: function (other) {
                return equalsUnions(this, other);
            }
        }, {
            key: "CompareTo",
            value: function (other) {
                return compareUnions(this, other);
            }
        }]);

        return AppEvent;
    }();

    setType("Fable.Arch.App.Types.AppEvent", AppEvent);

    var AppMessage = __exports.AppMessage = function () {
        function AppMessage(caseName, fields) {
            _classCallCheck$9(this, AppMessage);

            this.Case = caseName;
            this.Fields = fields;
        }

        _createClass$9(AppMessage, [{
            key: _Symbol.reflection,
            value: function () {
                return {
                    type: "Fable.Arch.App.Types.AppMessage",
                    interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
                    cases: {
                        Message: [GenericParam("TMessage")],
                        Replay: [GenericParam("TModel"), makeGeneric(List$1, {
                            T: Tuple(["string", GenericParam("TMessage")])
                        })]
                    }
                };
            }
        }, {
            key: "Equals",
            value: function (other) {
                return equalsUnions(this, other);
            }
        }, {
            key: "CompareTo",
            value: function (other) {
                return compareUnions(this, other);
            }
        }]);

        return AppMessage;
    }();

    setType("Fable.Arch.App.Types.AppMessage", AppMessage);

    var Plugin = __exports.Plugin = function () {
        function Plugin(producer, subscriber) {
            _classCallCheck$9(this, Plugin);

            this.Producer = producer;
            this.Subscriber = subscriber;
        }

        _createClass$9(Plugin, [{
            key: _Symbol.reflection,
            value: function () {
                return {
                    type: "Fable.Arch.App.Types.Plugin",
                    interfaces: ["FSharpRecord"],
                    properties: {
                        Producer: "function",
                        Subscriber: "function"
                    }
                };
            }
        }]);

        return Plugin;
    }();

    setType("Fable.Arch.App.Types.Plugin", Plugin);

    var AppSpecification = __exports.AppSpecification = function () {
        function AppSpecification(initState, view, update, initMessage, createRenderer, nodeSelector, producers, subscribers) {
            _classCallCheck$9(this, AppSpecification);

            this.InitState = initState;
            this.View = view;
            this.Update = update;
            this.InitMessage = initMessage;
            this.CreateRenderer = createRenderer;
            this.NodeSelector = nodeSelector;
            this.Producers = producers;
            this.Subscribers = subscribers;
        }

        _createClass$9(AppSpecification, [{
            key: _Symbol.reflection,
            value: function () {
                return {
                    type: "Fable.Arch.App.Types.AppSpecification",
                    interfaces: ["FSharpRecord"],
                    properties: {
                        InitState: GenericParam("TModel"),
                        View: "function",
                        Update: "function",
                        InitMessage: "function",
                        CreateRenderer: "function",
                        NodeSelector: "string",
                        Producers: makeGeneric(List$1, {
                            T: "function"
                        }),
                        Subscribers: makeGeneric(List$1, {
                            T: "function"
                        })
                    }
                };
            }
        }]);

        return AppSpecification;
    }();

    setType("Fable.Arch.App.Types.AppSpecification", AppSpecification);

    var App = __exports.App = function () {
        function App(model, actions, render, subscribers) {
            _classCallCheck$9(this, App);

            this.Model = model;
            this.Actions = actions;
            this.Render = render;
            this.Subscribers = subscribers;
        }

        _createClass$9(App, [{
            key: _Symbol.reflection,
            value: function () {
                return {
                    type: "Fable.Arch.App.Types.App",
                    interfaces: ["FSharpRecord"],
                    properties: {
                        Model: GenericParam("TModel"),
                        Actions: makeGeneric(List$1, {
                            T: "function"
                        }),
                        Render: "function",
                        Subscribers: makeGeneric(List$1, {
                            T: "function"
                        })
                    }
                };
            }
        }]);

        return App;
    }();

    setType("Fable.Arch.App.Types.App", App);

    var application = __exports.application = function (initMessage, handleMessage, handleReplay, configureProducers, createInitApp) {
        var state = null;

        var notifySubs = function notifySubs(msg) {
            if (state == null) {} else {
                var s = state;
                iterate(function (sub) {
                    sub(msg);
                }, s.Subscribers);
            }
        };

        var handleEvent = function handleEvent(evt) {
            var patternInput = evt.Case === "Replay" ? handleReplay(handleEvent)(notifySubs)([evt.Fields[0], evt.Fields[1]])(state) : handleMessage(handleEvent)(notifySubs)(evt.Fields[0])(state);
            state = patternInput[0];
            iterate(function (x) {
                x();
            }, patternInput[1]);
        };

        var post = function post($var2) {
            return handleEvent(function (arg0) {
                return new AppMessage("Message", [arg0]);
            }($var2));
        };

        state = createInitApp(post);
        initMessage(post);
        configureProducers(handleEvent);
        return handleEvent;
    };

    var render = __exports.render = function (post, viewFn, app) {
        var view = viewFn(app.Model);
        app.Render(function ($var3) {
            return post(function (arg0) {
                return new AppMessage("Message", [arg0]);
            }($var3));
        })(view);
        return app;
    };

    var createActions = __exports.createActions = function (post) {
        var mapping = function mapping(a) {
            return function () {
                return a(function ($var4) {
                    return post(function (arg0) {
                        return new AppMessage("Message", [arg0]);
                    }($var4));
                });
            };
        };

        return function (list) {
            return map$$1(mapping, list);
        };
    };

    var handleMessage = __exports.handleMessage = function (update, viewFn, post, notifySubs, message, app) {
        notifySubs(new AppEvent("ActionReceived", [message]));
        var patternInput = update(app.Model)(message);
        var modelChanged = new AppEvent("ModelChanged", [new ModelChanged(app.Model, message, patternInput[0])]);
        var actions = createActions(post)(patternInput[1]);

        var app_ = function (app_1) {
            return render(post, viewFn, app_1);
        }(new App(patternInput[0], app.Actions, app.Render, app.Subscribers));

        return [app_, new List$1(function () {
            notifySubs(modelChanged);
        }, actions)];
    };

    var calculateModelChanges = __exports.calculateModelChanges = function (initState, update, actions) {
        var execUpdate = function execUpdate(r) {
            return function (a) {
                var m = r.tail != null ? r.head[1] : initState;
                var msg = a[1];
                var patternInput = update(m)(a[1]);
                var id = a[0];
                return [id, patternInput[0]];
            };
        };

        return fold(function (s, a) {
            return new List$1(execUpdate(s)(a), s);
        }, new List$1(), actions);
    };

    var handleReplay = __exports.handleReplay = function (viewFn, updateFn, post, notifySubs, fromModel, actions, app) {
        var result = calculateModelChanges(fromModel, updateFn, actions);
        var model = result.tail == null ? fromModel : result.head[1];

        var app_ = function (app_1) {
            return render(post, viewFn, app_1);
        }(new App(model, app.Actions, app.Render, app.Subscribers));

        return [app_, ofArray([function () {
            return notifySubs(new AppEvent("Replayed", [result]));
        }])];
    };

    return __exports;
}({});
var AppApi = function (__exports) {
    var mapAction = __exports.mapAction = function (mapping, action, x) {
        action(function ($var5) {
            return x(mapping($var5));
        });
    };

    var mapAppMessage = __exports.mapAppMessage = function (map$$2, _arg1) {
        return _arg1.Case === "Replay" ? new Types$1.AppMessage("Replay", [_arg1.Fields[0], map$$1(function (tupledArg) {
            return [tupledArg[0], map$$2(tupledArg[1])];
        }, _arg1.Fields[1])]) : new Types$1.AppMessage("Message", [map$$2(_arg1.Fields[0])]);
    };

    var mapProducer = __exports.mapProducer = function (map$$2, p) {
        return function (x) {
            mapAction(map$$2, p, x);
        };
    };

    var mapSubscriber = __exports.mapSubscriber = function (mapModelChanged, mapAction_1, sub, _arg1) {
        if (_arg1.Case === "ActionReceived") {
            (function (option) {
                iterate(sub, function () {
                    var $var6 = option;

                    if ($var6 != null) {
                        return [$var6];
                    } else {
                        return [];
                    }
                }());
            })(function () {
                var $var7 = mapAction_1(function (x) {
                    return x;
                })(_arg1.Fields[0]);

                if ($var7 != null) {
                    return function (arg0) {
                        return new Types$1.AppEvent("ActionReceived", [arg0]);
                    }($var7);
                } else {
                    return $var7;
                }
            }());
        } else {
            if (_arg1.Case === "Replayed") {
                sub(new Types$1.AppEvent("Replayed", [_arg1.Fields[0]]));
            } else {
                (function (option) {
                    iterate(sub, function () {
                        var $var8 = option;

                        if ($var8 != null) {
                            return [$var8];
                        } else {
                            return [];
                        }
                    }());
                })(function () {
                    var $var9 = mapModelChanged(_arg1.Fields[0]);

                    if ($var9 != null) {
                        return function (arg0) {
                            return new Types$1.AppEvent("ModelChanged", [arg0]);
                        }($var9);
                    } else {
                        return $var9;
                    }
                }());
            }
        }
    };

    var mapActions = __exports.mapActions = function (m) {
        var mapping = function mapping(action) {
            return function (x) {
                mapAction(m, action, x);
            };
        };

        return function (list) {
            return map$$1(mapping, list);
        };
    };

    var toActionList = __exports.toActionList = function (a) {
        return ofArray([a]);
    };

    var createApp = __exports.createApp = function (state, view, update, createRenderer) {
        return new Types$1.AppSpecification(state, view, update, function (_arg1) {}, createRenderer, "body", new List$1(), new List$1());
    };

    var createSimpleApp = __exports.createSimpleApp = function (model, view, update) {
        var update_1 = function update_1(x) {
            return function (y) {
                return [update(x)(y), new List$1()];
            };
        };

        return function (createRenderer) {
            return createApp(model, view, update_1, createRenderer);
        };
    };

    var withStartNodeSelector = __exports.withStartNodeSelector = function (selector, app) {
        return new Types$1.AppSpecification(app.InitState, app.View, app.Update, app.InitMessage, app.CreateRenderer, selector, app.Producers, app.Subscribers);
    };

    var withInitMessage = __exports.withInitMessage = function (msg, app) {
        return new Types$1.AppSpecification(app.InitState, app.View, app.Update, msg, app.CreateRenderer, app.NodeSelector, app.Producers, app.Subscribers);
    };

    var withInstrumentationProducer = function withInstrumentationProducer(p, app) {
        var Producers = new List$1(p, app.Producers);
        return new Types$1.AppSpecification(app.InitState, app.View, app.Update, app.InitMessage, app.CreateRenderer, app.NodeSelector, Producers, app.Subscribers);
    };

    var withProducer = __exports.withProducer = function (producer, app) {
        var lift = function lift(h) {
            return function ($var10) {
                return h(function (arg0) {
                    return new Types$1.AppMessage("Message", [arg0]);
                }($var10));
            };
        };

        var producer_ = function producer_($var11) {
            return producer(lift($var11));
        };

        return withInstrumentationProducer(producer_, app);
    };

    var withInstrumentationSubscriber = __exports.withInstrumentationSubscriber = function (subscriber, app) {
        var Subscribers = new List$1(subscriber, app.Subscribers);
        return new Types$1.AppSpecification(app.InitState, app.View, app.Update, app.InitMessage, app.CreateRenderer, app.NodeSelector, app.Producers, Subscribers);
    };

    var withSubscriber = __exports.withSubscriber = function (subscriber, app) {
        var subscriber_ = function subscriber_(_arg1) {
            if (_arg1.Case === "ModelChanged") {
                subscriber(_arg1.Fields[0]);
            }
        };

        return withInstrumentationSubscriber(subscriber_, app);
    };

    var withPlugin = __exports.withPlugin = function (plugin) {
        return function ($var12) {
            return withInstrumentationProducer(plugin.Producer, withInstrumentationSubscriber(plugin.Subscriber, $var12));
        };
    };

    var configureProducers = __exports.configureProducers = function (producers, post) {
        iterate(function (p) {
            p(post);
        }, producers);
    };

    var start = __exports.start = function (appSpec) {
        var createInitApp = function createInitApp(post) {
            var view = appSpec.View(appSpec.InitState);
            var render = appSpec.CreateRenderer(appSpec.NodeSelector)(post)(view);
            return new Types$1.App(appSpec.InitState, new List$1(), render, appSpec.Subscribers);
        };

        var handleMessage_ = function handleMessage_(post) {
            return function (notifySubs) {
                return function (message) {
                    return function (app) {
                        return Types$1.handleMessage(appSpec.Update, appSpec.View, post, notifySubs, message, app);
                    };
                };
            };
        };

        var handleReplay_ = function handleReplay_(post) {
            return function (notifySubs) {
                return function (tupledArg) {
                    return function (app) {
                        return Types$1.handleReplay(appSpec.View, appSpec.Update, post, notifySubs, tupledArg[0], tupledArg[1], app);
                    };
                };
            };
        };

        var configureProducers_ = function configureProducers_(post) {
            configureProducers(appSpec.Producers, post);
        };

        return Types$1.application(appSpec.InitMessage, handleMessage_, handleReplay_, configureProducers_, createInitApp);
    };

    return __exports;
}({});

var nativeIsArray = Array.isArray;
var toString$1 = Object.prototype.toString;

var index$2 = nativeIsArray || isArray$2;

function isArray$2(obj) {
    return toString$1.call(obj) === "[object Array]"
}

var version$1 = "2";

var version = version$1;

VirtualPatch.NONE = 0;
VirtualPatch.VTEXT = 1;
VirtualPatch.VNODE = 2;
VirtualPatch.WIDGET = 3;
VirtualPatch.PROPS = 4;
VirtualPatch.ORDER = 5;
VirtualPatch.INSERT = 6;
VirtualPatch.REMOVE = 7;
VirtualPatch.THUNK = 8;

var vpatch = VirtualPatch;

function VirtualPatch(type, vNode, patch) {
    this.type = Number(type);
    this.vNode = vNode;
    this.patch = patch;
}

VirtualPatch.prototype.version = version;
VirtualPatch.prototype.type = "VirtualPatch";

var version$3 = version$1;

var isVnode = isVirtualNode;

function isVirtualNode(x) {
    return x && x.type === "VirtualNode" && x.version === version$3
}

var version$4 = version$1;

var isVtext = isVirtualText;

function isVirtualText(x) {
    return x && x.type === "VirtualText" && x.version === version$4
}

var isWidget_1 = isWidget$1;

function isWidget$1(w) {
    return w && w.type === "Widget"
}

var isThunk_1 = isThunk$1;

function isThunk$1(t) {
    return t && t.type === "Thunk"
}

var isVNode$1 = isVnode;
var isVText$1 = isVtext;
var isWidget$2 = isWidget_1;
var isThunk$2 = isThunk_1;

var handleThunk_1 = handleThunk$1;

function handleThunk$1(a, b) {
    var renderedA = a;
    var renderedB = b;

    if (isThunk$2(b)) {
        renderedB = renderThunk(b, a);
    }

    if (isThunk$2(a)) {
        renderedA = renderThunk(a, null);
    }

    return {
        a: renderedA,
        b: renderedB
    }
}

function renderThunk(thunk, previous) {
    var renderedThunk = thunk.vnode;

    if (!renderedThunk) {
        renderedThunk = thunk.vnode = thunk.render(previous);
    }

    if (!(isVNode$1(renderedThunk) ||
            isVText$1(renderedThunk) ||
            isWidget$2(renderedThunk))) {
        throw new Error("thunk did not return a valid node");
    }

    return renderedThunk
}

var index$4 = function isObject(x) {
	return typeof x === "object" && x !== null;
};

var isVhook = isHook$1;

function isHook$1(hook) {
    return hook &&
      (typeof hook.hook === "function" && !hook.hasOwnProperty("hook") ||
       typeof hook.unhook === "function" && !hook.hasOwnProperty("unhook"))
}

var isObject$1 = index$4;
var isHook = isVhook;

var diffProps_1 = diffProps$1;

function diffProps$1(a, b) {
    var diff;

    for (var aKey in a) {
        if (!(aKey in b)) {
            diff = diff || {};
            diff[aKey] = undefined;
        }

        var aValue = a[aKey];
        var bValue = b[aKey];

        if (aValue === bValue) {
            continue
        } else if (isObject$1(aValue) && isObject$1(bValue)) {
            if (getPrototype(bValue) !== getPrototype(aValue)) {
                diff = diff || {};
                diff[aKey] = bValue;
            } else if (isHook(bValue)) {
                 diff = diff || {};
                 diff[aKey] = bValue;
            } else {
                var objectDiff = diffProps$1(aValue, bValue);
                if (objectDiff) {
                    diff = diff || {};
                    diff[aKey] = objectDiff;
                }
            }
        } else {
            diff = diff || {};
            diff[aKey] = bValue;
        }
    }

    for (var bKey in b) {
        if (!(bKey in a)) {
            diff = diff || {};
            diff[bKey] = b[bKey];
        }
    }

    return diff
}

function getPrototype(value) {
  if (Object.getPrototypeOf) {
    return Object.getPrototypeOf(value)
  } else if (value.__proto__) {
    return value.__proto__
  } else if (value.constructor) {
    return value.constructor.prototype
  }
}

var isArray$1 = index$2;

var VPatch = vpatch;
var isVNode = isVnode;
var isVText = isVtext;
var isWidget = isWidget_1;
var isThunk = isThunk_1;
var handleThunk = handleThunk_1;

var diffProps = diffProps_1;

var diff_1$2 = diff$2;

function diff$2(a, b) {
    var patch = { a: a };
    walk(a, b, patch, 0);
    return patch
}

function walk(a, b, patch, index) {
    if (a === b) {
        return
    }

    var apply = patch[index];
    var applyClear = false;

    if (isThunk(a) || isThunk(b)) {
        thunks(a, b, patch, index);
    } else if (b == null) {

        // If a is a widget we will add a remove patch for it
        // Otherwise any child widgets/hooks must be destroyed.
        // This prevents adding two remove patches for a widget.
        if (!isWidget(a)) {
            clearState(a, patch, index);
            apply = patch[index];
        }

        apply = appendPatch(apply, new VPatch(VPatch.REMOVE, a, b));
    } else if (isVNode(b)) {
        if (isVNode(a)) {
            if (a.tagName === b.tagName &&
                a.namespace === b.namespace &&
                a.key === b.key) {
                var propsPatch = diffProps(a.properties, b.properties);
                if (propsPatch) {
                    apply = appendPatch(apply,
                        new VPatch(VPatch.PROPS, a, propsPatch));
                }
                apply = diffChildren(a, b, patch, apply, index);
            } else {
                apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b));
                applyClear = true;
            }
        } else {
            apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b));
            applyClear = true;
        }
    } else if (isVText(b)) {
        if (!isVText(a)) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b));
            applyClear = true;
        } else if (a.text !== b.text) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b));
        }
    } else if (isWidget(b)) {
        if (!isWidget(a)) {
            applyClear = true;
        }

        apply = appendPatch(apply, new VPatch(VPatch.WIDGET, a, b));
    }

    if (apply) {
        patch[index] = apply;
    }

    if (applyClear) {
        clearState(a, patch, index);
    }
}

function diffChildren(a, b, patch, apply, index) {
    var aChildren = a.children;
    var orderedSet = reorder(aChildren, b.children);
    var bChildren = orderedSet.children;

    var aLen = aChildren.length;
    var bLen = bChildren.length;
    var len = aLen > bLen ? aLen : bLen;

    for (var i = 0; i < len; i++) {
        var leftNode = aChildren[i];
        var rightNode = bChildren[i];
        index += 1;

        if (!leftNode) {
            if (rightNode) {
                // Excess nodes in b need to be added
                apply = appendPatch(apply,
                    new VPatch(VPatch.INSERT, null, rightNode));
            }
        } else {
            walk(leftNode, rightNode, patch, index);
        }

        if (isVNode(leftNode) && leftNode.count) {
            index += leftNode.count;
        }
    }

    if (orderedSet.moves) {
        // Reorder nodes last
        apply = appendPatch(apply, new VPatch(
            VPatch.ORDER,
            a,
            orderedSet.moves
        ));
    }

    return apply
}

function clearState(vNode, patch, index) {
    // TODO: Make this a single walk, not two
    unhook(vNode, patch, index);
    destroyWidgets(vNode, patch, index);
}

// Patch records for all destroyed widgets must be added because we need
// a DOM node reference for the destroy function
function destroyWidgets(vNode, patch, index) {
    if (isWidget(vNode)) {
        if (typeof vNode.destroy === "function") {
            patch[index] = appendPatch(
                patch[index],
                new VPatch(VPatch.REMOVE, vNode, null)
            );
        }
    } else if (isVNode(vNode) && (vNode.hasWidgets || vNode.hasThunks)) {
        var children = vNode.children;
        var len = children.length;
        for (var i = 0; i < len; i++) {
            var child = children[i];
            index += 1;

            destroyWidgets(child, patch, index);

            if (isVNode(child) && child.count) {
                index += child.count;
            }
        }
    } else if (isThunk(vNode)) {
        thunks(vNode, null, patch, index);
    }
}

// Create a sub-patch for thunks
function thunks(a, b, patch, index) {
    var nodes = handleThunk(a, b);
    var thunkPatch = diff$2(nodes.a, nodes.b);
    if (hasPatches(thunkPatch)) {
        patch[index] = new VPatch(VPatch.THUNK, null, thunkPatch);
    }
}

function hasPatches(patch) {
    for (var index in patch) {
        if (index !== "a") {
            return true
        }
    }

    return false
}

// Execute hooks when two nodes are identical
function unhook(vNode, patch, index) {
    if (isVNode(vNode)) {
        if (vNode.hooks) {
            patch[index] = appendPatch(
                patch[index],
                new VPatch(
                    VPatch.PROPS,
                    vNode,
                    undefinedKeys(vNode.hooks)
                )
            );
        }

        if (vNode.descendantHooks || vNode.hasThunks) {
            var children = vNode.children;
            var len = children.length;
            for (var i = 0; i < len; i++) {
                var child = children[i];
                index += 1;

                unhook(child, patch, index);

                if (isVNode(child) && child.count) {
                    index += child.count;
                }
            }
        }
    } else if (isThunk(vNode)) {
        thunks(vNode, null, patch, index);
    }
}

function undefinedKeys(obj) {
    var result = {};

    for (var key in obj) {
        result[key] = undefined;
    }

    return result
}

// List diff, naive left to right reordering
function reorder(aChildren, bChildren) {
    // O(M) time, O(M) memory
    var bChildIndex = keyIndex(bChildren);
    var bKeys = bChildIndex.keys;
    var bFree = bChildIndex.free;

    if (bFree.length === bChildren.length) {
        return {
            children: bChildren,
            moves: null
        }
    }

    // O(N) time, O(N) memory
    var aChildIndex = keyIndex(aChildren);
    var aKeys = aChildIndex.keys;
    var aFree = aChildIndex.free;

    if (aFree.length === aChildren.length) {
        return {
            children: bChildren,
            moves: null
        }
    }

    // O(MAX(N, M)) memory
    var newChildren = [];

    var freeIndex = 0;
    var freeCount = bFree.length;
    var deletedItems = 0;

    // Iterate through a and match a node in b
    // O(N) time,
    for (var i = 0 ; i < aChildren.length; i++) {
        var aItem = aChildren[i];
        var itemIndex;

        if (aItem.key) {
            if (bKeys.hasOwnProperty(aItem.key)) {
                // Match up the old keys
                itemIndex = bKeys[aItem.key];
                newChildren.push(bChildren[itemIndex]);

            } else {
                // Remove old keyed items
                itemIndex = i - deletedItems++;
                newChildren.push(null);
            }
        } else {
            // Match the item in a with the next free item in b
            if (freeIndex < freeCount) {
                itemIndex = bFree[freeIndex++];
                newChildren.push(bChildren[itemIndex]);
            } else {
                // There are no free items in b to match with
                // the free items in a, so the extra free nodes
                // are deleted.
                itemIndex = i - deletedItems++;
                newChildren.push(null);
            }
        }
    }

    var lastFreeIndex = freeIndex >= bFree.length ?
        bChildren.length :
        bFree[freeIndex];

    // Iterate through b and append any new keys
    // O(M) time
    for (var j = 0; j < bChildren.length; j++) {
        var newItem = bChildren[j];

        if (newItem.key) {
            if (!aKeys.hasOwnProperty(newItem.key)) {
                // Add any new keyed items
                // We are adding new items to the end and then sorting them
                // in place. In future we should insert new items in place.
                newChildren.push(newItem);
            }
        } else if (j >= lastFreeIndex) {
            // Add any leftover non-keyed items
            newChildren.push(newItem);
        }
    }

    var simulate = newChildren.slice();
    var simulateIndex = 0;
    var removes = [];
    var inserts = [];
    var simulateItem;

    for (var k = 0; k < bChildren.length;) {
        var wantedItem = bChildren[k];
        simulateItem = simulate[simulateIndex];

        // remove items
        while (simulateItem === null && simulate.length) {
            removes.push(remove$3(simulate, simulateIndex, null));
            simulateItem = simulate[simulateIndex];
        }

        if (!simulateItem || simulateItem.key !== wantedItem.key) {
            // if we need a key in this position...
            if (wantedItem.key) {
                if (simulateItem && simulateItem.key) {
                    // if an insert doesn't put this key in place, it needs to move
                    if (bKeys[simulateItem.key] !== k + 1) {
                        removes.push(remove$3(simulate, simulateIndex, simulateItem.key));
                        simulateItem = simulate[simulateIndex];
                        // if the remove didn't put the wanted item in place, we need to insert it
                        if (!simulateItem || simulateItem.key !== wantedItem.key) {
                            inserts.push({key: wantedItem.key, to: k});
                        }
                        // items are matching, so skip ahead
                        else {
                            simulateIndex++;
                        }
                    }
                    else {
                        inserts.push({key: wantedItem.key, to: k});
                    }
                }
                else {
                    inserts.push({key: wantedItem.key, to: k});
                }
                k++;
            }
            // a key in simulate has no matching wanted key, remove it
            else if (simulateItem && simulateItem.key) {
                removes.push(remove$3(simulate, simulateIndex, simulateItem.key));
            }
        }
        else {
            simulateIndex++;
            k++;
        }
    }

    // remove all the remaining nodes from simulate
    while(simulateIndex < simulate.length) {
        simulateItem = simulate[simulateIndex];
        removes.push(remove$3(simulate, simulateIndex, simulateItem && simulateItem.key));
    }

    // If the only moves we have are deletes then we can just
    // let the delete patch remove these items.
    if (removes.length === deletedItems && !inserts.length) {
        return {
            children: newChildren,
            moves: null
        }
    }

    return {
        children: newChildren,
        moves: {
            removes: removes,
            inserts: inserts
        }
    }
}

function remove$3(arr, index, key) {
    arr.splice(index, 1);

    return {
        from: index,
        key: key
    }
}

function keyIndex(children) {
    var keys = {};
    var free = [];
    var length = children.length;

    for (var i = 0; i < length; i++) {
        var child = children[i];

        if (child.key) {
            keys[child.key] = i;
        } else {
            free.push(i);
        }
    }

    return {
        keys: keys,     // A hash of key name to index
        free: free      // An array of unkeyed item indices
    }
}

function appendPatch(apply, patch) {
    if (apply) {
        if (isArray$1(apply)) {
            apply.push(patch);
        } else {
            apply = [apply, patch];
        }

        return apply
    } else {
        return patch
    }
}

var diff$1 = diff_1$2;

var diff_1 = diff$1;

var commonjsGlobal = typeof window !== 'undefined' ? window : typeof global !== 'undefined' ? global : typeof self !== 'undefined' ? self : {};





function createCommonjsModule(fn, module) {
	return module = { exports: {} }, fn(module, module.exports), module.exports;
}

var slice$1 = Array.prototype.slice;

var index$8 = iterativelyWalk;

function iterativelyWalk(nodes, cb) {
    if (!('length' in nodes)) {
        nodes = [nodes];
    }
    
    nodes = slice$1.call(nodes);

    while(nodes.length) {
        var node = nodes.shift(),
            ret = cb(node);

        if (ret) {
            return ret
        }

        if (node.childNodes && node.childNodes.length) {
            nodes = slice$1.call(node.childNodes).concat(nodes);
        }
    }
}

var domComment = Comment$1;

function Comment$1(data, owner) {
    if (!(this instanceof Comment$1)) {
        return new Comment$1(data, owner)
    }

    this.data = data;
    this.nodeValue = data;
    this.length = data.length;
    this.ownerDocument = owner || null;
}

Comment$1.prototype.nodeType = 8;
Comment$1.prototype.nodeName = "#comment";

Comment$1.prototype.toString = function _Comment_toString() {
    return "[object Comment]"
};

var domText = DOMText$1;

function DOMText$1(value, owner) {
    if (!(this instanceof DOMText$1)) {
        return new DOMText$1(value)
    }

    this.data = value || "";
    this.length = this.data.length;
    this.ownerDocument = owner || null;
}

DOMText$1.prototype.type = "DOMTextNode";
DOMText$1.prototype.nodeType = 3;
DOMText$1.prototype.nodeName = "#text";

DOMText$1.prototype.toString = function _Text_toString() {
    return this.data
};

DOMText$1.prototype.replaceData = function replaceData(index, length, value) {
    var current = this.data;
    var left = current.substring(0, index);
    var right = current.substring(index + length, current.length);
    this.data = left + value + right;
    this.length = this.data.length;
};

var dispatchEvent_1 = dispatchEvent$2;

function dispatchEvent$2(ev) {
    var elem = this;
    var type = ev.type;

    if (!ev.target) {
        ev.target = elem;
    }

    if (!elem.listeners) {
        elem.listeners = {};
    }

    var listeners = elem.listeners[type];

    if (listeners) {
        return listeners.forEach(function (listener) {
            ev.currentTarget = elem;
            if (typeof listener === 'function') {
                listener(ev);
            } else {
                listener.handleEvent(ev);
            }
        })
    }

    if (elem.parentNode) {
        elem.parentNode.dispatchEvent(ev);
    }
}

var addEventListener_1 = addEventListener$2;

function addEventListener$2(type, listener) {
    var elem = this;

    if (!elem.listeners) {
        elem.listeners = {};
    }

    if (!elem.listeners[type]) {
        elem.listeners[type] = [];
    }

    if (elem.listeners[type].indexOf(listener) === -1) {
        elem.listeners[type].push(listener);
    }
}

var removeEventListener_1 = removeEventListener$2;

function removeEventListener$2(type, listener) {
    var elem = this;

    if (!elem.listeners) {
        return
    }

    if (!elem.listeners[type]) {
        return
    }

    var list = elem.listeners[type];
    var index = list.indexOf(listener);
    if (index !== -1) {
        list.splice(index, 1);
    }
}

var serialize = serializeNode$1;

var voidElements = ["area","base","br","col","embed","hr","img","input","keygen","link","menuitem","meta","param","source","track","wbr"];

function serializeNode$1(node) {
    switch (node.nodeType) {
        case 3:
            return escapeText(node.data)
        case 8:
            return "<!--" + node.data + "-->"
        default:
            return serializeElement(node)
    }
}

function serializeElement(elem) {
    var strings = [];

    var tagname = elem.tagName;

    if (elem.namespaceURI === "http://www.w3.org/1999/xhtml") {
        tagname = tagname.toLowerCase();
    }

    strings.push("<" + tagname + properties(elem) + datasetify(elem));

    if (voidElements.indexOf(tagname) > -1) {
        strings.push(" />");
    } else {
        strings.push(">");

        if (elem.childNodes.length) {
            strings.push.apply(strings, elem.childNodes.map(serializeNode$1));
        } else if (elem.textContent || elem.innerText) {
            strings.push(escapeText(elem.textContent || elem.innerText));
        } else if (elem.innerHTML) {
            strings.push(elem.innerHTML);
        }

        strings.push("</" + tagname + ">");
    }

    return strings.join("")
}

function isProperty(elem, key) {
    var type = typeof elem[key];

    if (key === "style" && Object.keys(elem.style).length > 0) {
      return true
    }

    return elem.hasOwnProperty(key) &&
        (type === "string" || type === "boolean" || type === "number") &&
        key !== "nodeName" && key !== "className" && key !== "tagName" &&
        key !== "textContent" && key !== "innerText" && key !== "namespaceURI" &&  key !== "innerHTML"
}

function stylify(styles) {
    if (typeof styles === 'string') return styles
    var attr = "";
    Object.keys(styles).forEach(function (key) {
        var value = styles[key];
        key = key.replace(/[A-Z]/g, function(c) {
            return "-" + c.toLowerCase();
        });
        attr += key + ":" + value + ";";
    });
    return attr
}

function datasetify(elem) {
    var ds = elem.dataset;
    var props = [];

    for (var key in ds) {
        props.push({ name: "data-" + key, value: ds[key] });
    }

    return props.length ? stringify(props) : ""
}

function stringify(list) {
    var attributes = [];
    list.forEach(function (tuple) {
        var name = tuple.name;
        var value = tuple.value;

        if (name === "style") {
            value = stylify(value);
        }

        attributes.push(name + "=" + "\"" + escapeAttributeValue(value) + "\"");
    });

    return attributes.length ? " " + attributes.join(" ") : ""
}

function properties(elem) {
    var props = [];
    for (var key in elem) {
        if (isProperty(elem, key)) {
            props.push({ name: key, value: elem[key] });
        }
    }

    for (var ns in elem._attributes) {
      for (var attribute in elem._attributes[ns]) {
        var prop = elem._attributes[ns][attribute];
        var name = (prop.prefix ? prop.prefix + ":" : "") + attribute;
        props.push({ name: name, value: prop.value });
      }
    }

    if (elem.className) {
        props.push({ name: "class", value: elem.className });
    }

    return props.length ? stringify(props) : ""
}

function escapeText(s) {
    var str = '';

    if (typeof(s) === 'string') { 
        str = s; 
    } else if (s) {
        str = s.toString();
    }

    return str
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
}

function escapeAttributeValue(str) {
    return escapeText(str).replace(/"/g, "&quot;")
}

var domWalk$1 = index$8;
var dispatchEvent$1 = dispatchEvent_1;
var addEventListener$1 = addEventListener_1;
var removeEventListener$1 = removeEventListener_1;
var serializeNode = serialize;

var htmlns = "http://www.w3.org/1999/xhtml";

var domElement = DOMElement$1;

function DOMElement$1(tagName, owner, namespace) {
    if (!(this instanceof DOMElement$1)) {
        return new DOMElement$1(tagName)
    }

    var ns = namespace === undefined ? htmlns : (namespace || null);

    this.tagName = ns === htmlns ? String(tagName).toUpperCase() : tagName;
    this.nodeName = this.tagName;
    this.className = "";
    this.dataset = {};
    this.childNodes = [];
    this.parentNode = null;
    this.style = {};
    this.ownerDocument = owner || null;
    this.namespaceURI = ns;
    this._attributes = {};

    if (this.tagName === 'INPUT') {
      this.type = 'text';
    }
}

DOMElement$1.prototype.type = "DOMElement";
DOMElement$1.prototype.nodeType = 1;

DOMElement$1.prototype.appendChild = function _Element_appendChild(child) {
    if (child.parentNode) {
        child.parentNode.removeChild(child);
    }

    this.childNodes.push(child);
    child.parentNode = this;

    return child
};

DOMElement$1.prototype.replaceChild =
    function _Element_replaceChild(elem, needle) {
        // TODO: Throw NotFoundError if needle.parentNode !== this

        if (elem.parentNode) {
            elem.parentNode.removeChild(elem);
        }

        var index = this.childNodes.indexOf(needle);

        needle.parentNode = null;
        this.childNodes[index] = elem;
        elem.parentNode = this;

        return needle
    };

DOMElement$1.prototype.removeChild = function _Element_removeChild(elem) {
    // TODO: Throw NotFoundError if elem.parentNode !== this

    var index = this.childNodes.indexOf(elem);
    this.childNodes.splice(index, 1);

    elem.parentNode = null;
    return elem
};

DOMElement$1.prototype.insertBefore =
    function _Element_insertBefore(elem, needle) {
        // TODO: Throw NotFoundError if referenceElement is a dom node
        // and parentNode !== this

        if (elem.parentNode) {
            elem.parentNode.removeChild(elem);
        }

        var index = needle === null || needle === undefined ?
            -1 :
            this.childNodes.indexOf(needle);

        if (index > -1) {
            this.childNodes.splice(index, 0, elem);
        } else {
            this.childNodes.push(elem);
        }

        elem.parentNode = this;
        return elem
    };

DOMElement$1.prototype.setAttributeNS =
    function _Element_setAttributeNS(namespace, name, value) {
        var prefix = null;
        var localName = name;
        var colonPosition = name.indexOf(":");
        if (colonPosition > -1) {
            prefix = name.substr(0, colonPosition);
            localName = name.substr(colonPosition + 1);
        }
        if (this.tagName === 'INPUT' && name === 'type') {
          this.type = value;
        }
        else {
          var attributes = this._attributes[namespace] || (this._attributes[namespace] = {});
          attributes[localName] = {value: value, prefix: prefix};
        }
    };

DOMElement$1.prototype.getAttributeNS =
    function _Element_getAttributeNS(namespace, name) {
        var attributes = this._attributes[namespace];
        var value = attributes && attributes[name] && attributes[name].value;
        if (this.tagName === 'INPUT' && name === 'type') {
          return this.type;
        }
        if (typeof value !== "string") {
            return null
        }
        return value
    };

DOMElement$1.prototype.removeAttributeNS =
    function _Element_removeAttributeNS(namespace, name) {
        var attributes = this._attributes[namespace];
        if (attributes) {
            delete attributes[name];
        }
    };

DOMElement$1.prototype.hasAttributeNS =
    function _Element_hasAttributeNS(namespace, name) {
        var attributes = this._attributes[namespace];
        return !!attributes && name in attributes;
    };

DOMElement$1.prototype.setAttribute = function _Element_setAttribute(name, value) {
    return this.setAttributeNS(null, name, value)
};

DOMElement$1.prototype.getAttribute = function _Element_getAttribute(name) {
    return this.getAttributeNS(null, name)
};

DOMElement$1.prototype.removeAttribute = function _Element_removeAttribute(name) {
    return this.removeAttributeNS(null, name)
};

DOMElement$1.prototype.hasAttribute = function _Element_hasAttribute(name) {
    return this.hasAttributeNS(null, name)
};

DOMElement$1.prototype.removeEventListener = removeEventListener$1;
DOMElement$1.prototype.addEventListener = addEventListener$1;
DOMElement$1.prototype.dispatchEvent = dispatchEvent$1;

// Un-implemented
DOMElement$1.prototype.focus = function _Element_focus() {
    return void 0
};

DOMElement$1.prototype.toString = function _Element_toString() {
    return serializeNode(this)
};

DOMElement$1.prototype.getElementsByClassName = function _Element_getElementsByClassName(classNames) {
    var classes = classNames.split(" ");
    var elems = [];

    domWalk$1(this, function (node) {
        if (node.nodeType === 1) {
            var nodeClassName = node.className || "";
            var nodeClasses = nodeClassName.split(" ");

            if (classes.every(function (item) {
                return nodeClasses.indexOf(item) !== -1
            })) {
                elems.push(node);
            }
        }
    });

    return elems
};

DOMElement$1.prototype.getElementsByTagName = function _Element_getElementsByTagName(tagName) {
    tagName = tagName.toLowerCase();
    var elems = [];

    domWalk$1(this.childNodes, function (node) {
        if (node.nodeType === 1 && (tagName === '*' || node.tagName.toLowerCase() === tagName)) {
            elems.push(node);
        }
    });

    return elems
};

DOMElement$1.prototype.contains = function _Element_contains(element) {
    return domWalk$1(this, function (node) {
        return element === node
    }) || false
};

var DOMElement$2 = domElement;

var domFragment = DocumentFragment$1;

function DocumentFragment$1(owner) {
    if (!(this instanceof DocumentFragment$1)) {
        return new DocumentFragment$1()
    }

    this.childNodes = [];
    this.parentNode = null;
    this.ownerDocument = owner || null;
}

DocumentFragment$1.prototype.type = "DocumentFragment";
DocumentFragment$1.prototype.nodeType = 11;
DocumentFragment$1.prototype.nodeName = "#document-fragment";

DocumentFragment$1.prototype.appendChild  = DOMElement$2.prototype.appendChild;
DocumentFragment$1.prototype.replaceChild = DOMElement$2.prototype.replaceChild;
DocumentFragment$1.prototype.removeChild  = DOMElement$2.prototype.removeChild;

DocumentFragment$1.prototype.toString =
    function _DocumentFragment_toString() {
        return this.childNodes.map(function (node) {
            return String(node)
        }).join("")
    };

var event = Event$1;

function Event$1(family) {}

Event$1.prototype.initEvent = function _Event_initEvent(type, bubbles, cancelable) {
    this.type = type;
    this.bubbles = bubbles;
    this.cancelable = cancelable;
};

Event$1.prototype.preventDefault = function _Event_preventDefault() {
    
};

var domWalk = index$8;

var Comment = domComment;
var DOMText = domText;
var DOMElement = domElement;
var DocumentFragment = domFragment;
var Event = event;
var dispatchEvent = dispatchEvent_1;
var addEventListener = addEventListener_1;
var removeEventListener = removeEventListener_1;

var document$2 = Document$1;

function Document$1() {
    if (!(this instanceof Document$1)) {
        return new Document$1();
    }

    this.head = this.createElement("head");
    this.body = this.createElement("body");
    this.documentElement = this.createElement("html");
    this.documentElement.appendChild(this.head);
    this.documentElement.appendChild(this.body);
    this.childNodes = [this.documentElement];
    this.nodeType = 9;
}

var proto = Document$1.prototype;
proto.createTextNode = function createTextNode(value) {
    return new DOMText(value, this)
};

proto.createElementNS = function createElementNS(namespace, tagName) {
    var ns = namespace === null ? null : String(namespace);
    return new DOMElement(tagName, this, ns)
};

proto.createElement = function createElement(tagName) {
    return new DOMElement(tagName, this)
};

proto.createDocumentFragment = function createDocumentFragment() {
    return new DocumentFragment(this)
};

proto.createEvent = function createEvent(family) {
    return new Event(family)
};

proto.createComment = function createComment(data) {
    return new Comment(data, this)
};

proto.getElementById = function getElementById(id) {
    id = String(id);

    var result = domWalk(this.childNodes, function (node) {
        if (String(node.id) === id) {
            return node
        }
    });

    return result || null
};

proto.getElementsByClassName = DOMElement.prototype.getElementsByClassName;
proto.getElementsByTagName = DOMElement.prototype.getElementsByTagName;
proto.contains = DOMElement.prototype.contains;

proto.removeEventListener = removeEventListener;
proto.addEventListener = addEventListener;
proto.dispatchEvent = dispatchEvent;

var Document = document$2;

var index$6 = new Document();

var document_1 = createCommonjsModule(function (module) {
var topLevel = typeof commonjsGlobal !== 'undefined' ? commonjsGlobal :
    typeof window !== 'undefined' ? window : {};
var minDoc = index$6;

if (typeof document !== 'undefined') {
    module.exports = document;
} else {
    var doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'];

    if (!doccy) {
        doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'] = minDoc;
    }

    module.exports = doccy;
}
});

var isObject$2 = index$4;
var isHook$2 = isVhook;

var applyProperties_1 = applyProperties$1;

function applyProperties$1(node, props, previous) {
    for (var propName in props) {
        var propValue = props[propName];

        if (propValue === undefined) {
            removeProperty(node, propName, propValue, previous);
        } else if (isHook$2(propValue)) {
            removeProperty(node, propName, propValue, previous);
            if (propValue.hook) {
                propValue.hook(node,
                    propName,
                    previous ? previous[propName] : undefined);
            }
        } else {
            if (isObject$2(propValue)) {
                patchObject(node, props, previous, propName, propValue);
            } else {
                node[propName] = propValue;
            }
        }
    }
}

function removeProperty(node, propName, propValue, previous) {
    if (previous) {
        var previousValue = previous[propName];

        if (!isHook$2(previousValue)) {
            if (propName === "attributes") {
                for (var attrName in previousValue) {
                    node.removeAttribute(attrName);
                }
            } else if (propName === "style") {
                for (var i in previousValue) {
                    node.style[i] = "";
                }
            } else if (typeof previousValue === "string") {
                node[propName] = "";
            } else {
                node[propName] = null;
            }
        } else if (previousValue.unhook) {
            previousValue.unhook(node, propName, propValue);
        }
    }
}

function patchObject(node, props, previous, propName, propValue) {
    var previousValue = previous ? previous[propName] : undefined;

    // Set attributes
    if (propName === "attributes") {
        for (var attrName in propValue) {
            var attrValue = propValue[attrName];

            if (attrValue === undefined) {
                node.removeAttribute(attrName);
            } else {
                node.setAttribute(attrName, attrValue);
            }
        }

        return
    }

    if(previousValue && isObject$2(previousValue) &&
        getPrototype$1(previousValue) !== getPrototype$1(propValue)) {
        node[propName] = propValue;
        return
    }

    if (!isObject$2(node[propName])) {
        node[propName] = {};
    }

    var replacer = propName === "style" ? "" : undefined;

    for (var k in propValue) {
        var value = propValue[k];
        node[propName][k] = (value === undefined) ? replacer : value;
    }
}

function getPrototype$1(value) {
    if (Object.getPrototypeOf) {
        return Object.getPrototypeOf(value)
    } else if (value.__proto__) {
        return value.__proto__
    } else if (value.constructor) {
        return value.constructor.prototype
    }
}

var document$4 = document_1;

var applyProperties = applyProperties_1;

var isVNode$2 = isVnode;
var isVText$2 = isVtext;
var isWidget$3 = isWidget_1;
var handleThunk$2 = handleThunk_1;

var createElement_1 = createElement$1;

function createElement$1(vnode, opts) {
    var doc = opts ? opts.document || document$4 : document$4;
    var warn = opts ? opts.warn : null;

    vnode = handleThunk$2(vnode).a;

    if (isWidget$3(vnode)) {
        return vnode.init()
    } else if (isVText$2(vnode)) {
        return doc.createTextNode(vnode.text)
    } else if (!isVNode$2(vnode)) {
        if (warn) {
            warn("Item is not a valid virtual dom node", vnode);
        }
        return null
    }

    var node = (vnode.namespace === null) ?
        doc.createElement(vnode.tagName) :
        doc.createElementNS(vnode.namespace, vnode.tagName);

    var props = vnode.properties;
    applyProperties(node, props);

    var children = vnode.children;

    for (var i = 0; i < children.length; i++) {
        var childNode = createElement$1(children[i], opts);
        if (childNode) {
            node.appendChild(childNode);
        }
    }

    return node
}

// Maps a virtual DOM tree onto a real DOM tree in an efficient manner.
// We don't want to read all of the DOM nodes in the tree so we use
// the in-order tree indexing to eliminate recursion down certain branches.
// We only recurse into a DOM node if we know that it contains a child of
// interest.

var noChild = {};

var domIndex_1 = domIndex$1;

function domIndex$1(rootNode, tree, indices, nodes) {
    if (!indices || indices.length === 0) {
        return {}
    } else {
        indices.sort(ascending);
        return recurse(rootNode, tree, indices, nodes, 0)
    }
}

function recurse(rootNode, tree, indices, nodes, rootIndex) {
    nodes = nodes || {};


    if (rootNode) {
        if (indexInRange(indices, rootIndex, rootIndex)) {
            nodes[rootIndex] = rootNode;
        }

        var vChildren = tree.children;

        if (vChildren) {

            var childNodes = rootNode.childNodes;

            for (var i = 0; i < tree.children.length; i++) {
                rootIndex += 1;

                var vChild = vChildren[i] || noChild;
                var nextIndex = rootIndex + (vChild.count || 0);

                // skip recursion down the tree if there are no nodes down here
                if (indexInRange(indices, rootIndex, nextIndex)) {
                    recurse(childNodes[i], vChild, indices, nodes, rootIndex);
                }

                rootIndex = nextIndex;
            }
        }
    }

    return nodes
}

// Binary search for an index in the interval [left, right]
function indexInRange(indices, left, right) {
    if (indices.length === 0) {
        return false
    }

    var minIndex = 0;
    var maxIndex = indices.length - 1;
    var currentIndex;
    var currentItem;

    while (minIndex <= maxIndex) {
        currentIndex = ((maxIndex + minIndex) / 2) >> 0;
        currentItem = indices[currentIndex];

        if (minIndex === maxIndex) {
            return currentItem >= left && currentItem <= right
        } else if (currentItem < left) {
            minIndex = currentIndex + 1;
        } else  if (currentItem > right) {
            maxIndex = currentIndex - 1;
        } else {
            return true
        }
    }

    return false;
}

function ascending(a, b) {
    return a > b ? 1 : -1
}

var isWidget$5 = isWidget_1;

var updateWidget_1 = updateWidget$1;

function updateWidget$1(a, b) {
    if (isWidget$5(a) && isWidget$5(b)) {
        if ("name" in a && "name" in b) {
            return a.id === b.id
        } else {
            return a.init === b.init
        }
    }

    return false
}

var applyProperties$2 = applyProperties_1;

var isWidget$4 = isWidget_1;
var VPatch$1 = vpatch;

var updateWidget = updateWidget_1;

var patchOp$1 = applyPatch$1;

function applyPatch$1(vpatch$$1, domNode, renderOptions) {
    var type = vpatch$$1.type;
    var vNode = vpatch$$1.vNode;
    var patch = vpatch$$1.patch;

    switch (type) {
        case VPatch$1.REMOVE:
            return removeNode(domNode, vNode)
        case VPatch$1.INSERT:
            return insertNode(domNode, patch, renderOptions)
        case VPatch$1.VTEXT:
            return stringPatch(domNode, vNode, patch, renderOptions)
        case VPatch$1.WIDGET:
            return widgetPatch(domNode, vNode, patch, renderOptions)
        case VPatch$1.VNODE:
            return vNodePatch(domNode, vNode, patch, renderOptions)
        case VPatch$1.ORDER:
            reorderChildren(domNode, patch);
            return domNode
        case VPatch$1.PROPS:
            applyProperties$2(domNode, patch, vNode.properties);
            return domNode
        case VPatch$1.THUNK:
            return replaceRoot(domNode,
                renderOptions.patch(domNode, patch, renderOptions))
        default:
            return domNode
    }
}

function removeNode(domNode, vNode) {
    var parentNode = domNode.parentNode;

    if (parentNode) {
        parentNode.removeChild(domNode);
    }

    destroyWidget(domNode, vNode);

    return null
}

function insertNode(parentNode, vNode, renderOptions) {
    var newNode = renderOptions.render(vNode, renderOptions);

    if (parentNode) {
        parentNode.appendChild(newNode);
    }

    return parentNode
}

function stringPatch(domNode, leftVNode, vText, renderOptions) {
    var newNode;

    if (domNode.nodeType === 3) {
        domNode.replaceData(0, domNode.length, vText.text);
        newNode = domNode;
    } else {
        var parentNode = domNode.parentNode;
        newNode = renderOptions.render(vText, renderOptions);

        if (parentNode && newNode !== domNode) {
            parentNode.replaceChild(newNode, domNode);
        }
    }

    return newNode
}

function widgetPatch(domNode, leftVNode, widget, renderOptions) {
    var updating = updateWidget(leftVNode, widget);
    var newNode;

    if (updating) {
        newNode = widget.update(leftVNode, domNode) || domNode;
    } else {
        newNode = renderOptions.render(widget, renderOptions);
    }

    var parentNode = domNode.parentNode;

    if (parentNode && newNode !== domNode) {
        parentNode.replaceChild(newNode, domNode);
    }

    if (!updating) {
        destroyWidget(domNode, leftVNode);
    }

    return newNode
}

function vNodePatch(domNode, leftVNode, vNode, renderOptions) {
    var parentNode = domNode.parentNode;
    var newNode = renderOptions.render(vNode, renderOptions);

    if (parentNode && newNode !== domNode) {
        parentNode.replaceChild(newNode, domNode);
    }

    return newNode
}

function destroyWidget(domNode, w) {
    if (typeof w.destroy === "function" && isWidget$4(w)) {
        w.destroy(domNode);
    }
}

function reorderChildren(domNode, moves) {
    var childNodes = domNode.childNodes;
    var keyMap = {};
    var node;
    var remove;
    var insert;

    for (var i = 0; i < moves.removes.length; i++) {
        remove = moves.removes[i];
        node = childNodes[remove.from];
        if (remove.key) {
            keyMap[remove.key] = node;
        }
        domNode.removeChild(node);
    }

    var length = childNodes.length;
    for (var j = 0; j < moves.inserts.length; j++) {
        insert = moves.inserts[j];
        node = keyMap[insert.key];
        // this is the weirdest bug i've ever seen in webkit
        domNode.insertBefore(node, insert.to >= length++ ? null : childNodes[insert.to]);
    }
}

function replaceRoot(oldRoot, newRoot) {
    if (oldRoot && newRoot && oldRoot !== newRoot && oldRoot.parentNode) {
        oldRoot.parentNode.replaceChild(newRoot, oldRoot);
    }

    return newRoot;
}

var document$1 = document_1;
var isArray$3 = index$2;

var render$1 = createElement_1;
var domIndex = domIndex_1;
var patchOp = patchOp$1;
var patch_1$2 = patch$2;

function patch$2(rootNode, patches, renderOptions) {
    renderOptions = renderOptions || {};
    renderOptions.patch = renderOptions.patch && renderOptions.patch !== patch$2
        ? renderOptions.patch
        : patchRecursive;
    renderOptions.render = renderOptions.render || render$1;

    return renderOptions.patch(rootNode, patches, renderOptions)
}

function patchRecursive(rootNode, patches, renderOptions) {
    var indices = patchIndices(patches);

    if (indices.length === 0) {
        return rootNode
    }

    var index = domIndex(rootNode, patches.a, indices);
    var ownerDocument = rootNode.ownerDocument;

    if (!renderOptions.document && ownerDocument !== document$1) {
        renderOptions.document = ownerDocument;
    }

    for (var i = 0; i < indices.length; i++) {
        var nodeIndex = indices[i];
        rootNode = applyPatch(rootNode,
            index[nodeIndex],
            patches[nodeIndex],
            renderOptions);
    }

    return rootNode
}

function applyPatch(rootNode, domNode, patchList, renderOptions) {
    if (!domNode) {
        return rootNode
    }

    var newNode;

    if (isArray$3(patchList)) {
        for (var i = 0; i < patchList.length; i++) {
            newNode = patchOp(patchList[i], domNode, renderOptions);

            if (domNode === rootNode) {
                rootNode = newNode;
            }
        }
    } else {
        newNode = patchOp(patchList, domNode, renderOptions);

        if (domNode === rootNode) {
            rootNode = newNode;
        }
    }

    return rootNode
}

function patchIndices(patches) {
    var indices = [];

    for (var key in patches) {
        if (key !== "a") {
            indices.push(Number(key));
        }
    }

    return indices
}

var patch$1 = patch_1$2;

var patch_1 = patch$1;

var version$5 = version$1;
var isVNode$4 = isVnode;
var isWidget$7 = isWidget_1;
var isThunk$3 = isThunk_1;
var isVHook = isVhook;

var vnode = VirtualNode;

var noProperties = {};
var noChildren = [];

function VirtualNode(tagName, properties, children, key, namespace) {
    this.tagName = tagName;
    this.properties = properties || noProperties;
    this.children = children || noChildren;
    this.key = key != null ? String(key) : undefined;
    this.namespace = (typeof namespace === "string") ? namespace : null;

    var count = (children && children.length) || 0;
    var descendants = 0;
    var hasWidgets = false;
    var hasThunks = false;
    var descendantHooks = false;
    var hooks;

    for (var propName in properties) {
        if (properties.hasOwnProperty(propName)) {
            var property = properties[propName];
            if (isVHook(property) && property.unhook) {
                if (!hooks) {
                    hooks = {};
                }

                hooks[propName] = property;
            }
        }
    }

    for (var i = 0; i < count; i++) {
        var child = children[i];
        if (isVNode$4(child)) {
            descendants += child.count || 0;

            if (!hasWidgets && child.hasWidgets) {
                hasWidgets = true;
            }

            if (!hasThunks && child.hasThunks) {
                hasThunks = true;
            }

            if (!descendantHooks && (child.hooks || child.descendantHooks)) {
                descendantHooks = true;
            }
        } else if (!hasWidgets && isWidget$7(child)) {
            if (typeof child.destroy === "function") {
                hasWidgets = true;
            }
        } else if (!hasThunks && isThunk$3(child)) {
            hasThunks = true;
        }
    }

    this.count = count + descendants;
    this.hasWidgets = hasWidgets;
    this.hasThunks = hasThunks;
    this.hooks = hooks;
    this.descendantHooks = descendantHooks;
}

VirtualNode.prototype.version = version$5;
VirtualNode.prototype.type = "VirtualNode";

var version$6 = version$1;

var vtext = VirtualText;

function VirtualText(text) {
    this.text = String(text);
}

VirtualText.prototype.version = version$6;
VirtualText.prototype.type = "VirtualText";

/*!
 * Cross-Browser Split 1.1.1
 * Copyright 2007-2012 Steven Levithan <stevenlevithan.com>
 * Available under the MIT License
 * ECMAScript compliant, uniform cross-browser split method
 */

/**
 * Splits a string into an array of strings using a regex or string separator. Matches of the
 * separator are not included in the result array. However, if `separator` is a regex that contains
 * capturing groups, backreferences are spliced into the result each time `separator` is matched.
 * Fixes browser bugs compared to the native `String.prototype.split` and can be used reliably
 * cross-browser.
 * @param {String} str String to split.
 * @param {RegExp|String} separator Regex or string to use for separating the string.
 * @param {Number} [limit] Maximum number of items to include in the result array.
 * @returns {Array} Array of substrings.
 * @example
 *
 * // Basic use
 * split('a b c d', ' ');
 * // -> ['a', 'b', 'c', 'd']
 *
 * // With limit
 * split('a b c d', ' ', 2);
 * // -> ['a', 'b']
 *
 * // Backreferences in result array
 * split('..word1 word2..', /([a-z]+)(\d+)/i);
 * // -> ['..', 'word', '1', ' ', 'word', '2', '..']
 */
var index$12 = (function split(undef) {

  var nativeSplit = String.prototype.split,
    compliantExecNpcg = /()??/.exec("")[1] === undef,
    // NPCG: nonparticipating capturing group
    self;

  self = function(str, separator, limit) {
    // If `separator` is not a regex, use `nativeSplit`
    if (Object.prototype.toString.call(separator) !== "[object RegExp]") {
      return nativeSplit.call(str, separator, limit);
    }
    var output = [],
      flags = (separator.ignoreCase ? "i" : "") + (separator.multiline ? "m" : "") + (separator.extended ? "x" : "") + // Proposed for ES6
      (separator.sticky ? "y" : ""),
      // Firefox 3+
      lastLastIndex = 0,
      // Make `global` and avoid `lastIndex` issues by working with a copy
      separator = new RegExp(separator.source, flags + "g"),
      separator2, match, lastIndex, lastLength;
    str += ""; // Type-convert
    if (!compliantExecNpcg) {
      // Doesn't need flags gy, but they don't hurt
      separator2 = new RegExp("^" + separator.source + "$(?!\\s)", flags);
    }
    /* Values for `limit`, per the spec:
     * If undefined: 4294967295 // Math.pow(2, 32) - 1
     * If 0, Infinity, or NaN: 0
     * If positive number: limit = Math.floor(limit); if (limit > 4294967295) limit -= 4294967296;
     * If negative number: 4294967296 - Math.floor(Math.abs(limit))
     * If other: Type-convert, then use the above rules
     */
    limit = limit === undef ? -1 >>> 0 : // Math.pow(2, 32) - 1
    limit >>> 0; // ToUint32(limit)
    while (match = separator.exec(str)) {
      // `separator.lastIndex` is not reliable cross-browser
      lastIndex = match.index + match[0].length;
      if (lastIndex > lastLastIndex) {
        output.push(str.slice(lastLastIndex, match.index));
        // Fix browsers whose `exec` methods don't consistently return `undefined` for
        // nonparticipating capturing groups
        if (!compliantExecNpcg && match.length > 1) {
          match[0].replace(separator2, function() {
            for (var i = 1; i < arguments.length - 2; i++) {
              if (arguments[i] === undef) {
                match[i] = undef;
              }
            }
          });
        }
        if (match.length > 1 && match.index < str.length) {
          Array.prototype.push.apply(output, match.slice(1));
        }
        lastLength = match[0].length;
        lastLastIndex = lastIndex;
        if (output.length >= limit) {
          break;
        }
      }
      if (separator.lastIndex === match.index) {
        separator.lastIndex++; // Avoid an infinite loop
      }
    }
    if (lastLastIndex === str.length) {
      if (lastLength || !separator.test("")) {
        output.push("");
      }
    } else {
      output.push(str.slice(lastLastIndex));
    }
    return output.length > limit ? output.slice(0, limit) : output;
  };

  return self;
})();

var split$3 = index$12;

var classIdSplit = /([\.#]?[a-zA-Z0-9\u007F-\uFFFF_:-]+)/;
var notClassId = /^\.|#/;

var parseTag_1 = parseTag$1;

function parseTag$1(tag, props) {
    if (!tag) {
        return 'DIV';
    }

    var noId = !(props.hasOwnProperty('id'));

    var tagParts = split$3(tag, classIdSplit);
    var tagName = null;

    if (notClassId.test(tagParts[1])) {
        tagName = 'DIV';
    }

    var classes, part, type, i;

    for (i = 0; i < tagParts.length; i++) {
        part = tagParts[i];

        if (!part) {
            continue;
        }

        type = part.charAt(0);

        if (!tagName) {
            tagName = part;
        } else if (type === '.') {
            classes = classes || [];
            classes.push(part.substring(1, part.length));
        } else if (type === '#' && noId) {
            props.id = part.substring(1, part.length);
        }
    }

    if (classes) {
        if (props.className) {
            classes.push(props.className);
        }

        props.className = classes.join(' ');
    }

    return props.namespace ? tagName : tagName.toUpperCase();
}

var softSetHook$1 = SoftSetHook;

function SoftSetHook(value) {
    if (!(this instanceof SoftSetHook)) {
        return new SoftSetHook(value);
    }

    this.value = value;
}

SoftSetHook.prototype.hook = function (node, propertyName) {
    if (node[propertyName] !== this.value) {
        node[propertyName] = this.value;
    }
};

/*global window, global*/

var root = typeof window !== 'undefined' ?
    window : typeof commonjsGlobal !== 'undefined' ?
    commonjsGlobal : {};

var index$16 = Individual$1;

function Individual$1(key, value) {
    if (key in root) {
        return root[key];
    }

    root[key] = value;

    return value;
}

var Individual = index$16;

var oneVersion = OneVersion;

function OneVersion(moduleName, version, defaultValue) {
    var key = '__INDIVIDUAL_ONE_VERSION_' + moduleName;
    var enforceKey = key + '_ENFORCE_SINGLETON';

    var versionValue = Individual(enforceKey, version);

    if (versionValue !== version) {
        throw new Error('Can only have one copy of ' +
            moduleName + '.\n' +
            'You already have version ' + versionValue +
            ' installed.\n' +
            'This means you cannot install version ' + version);
    }

    return Individual(key, defaultValue);
}

var OneVersionConstraint = oneVersion;

var MY_VERSION = '7';
OneVersionConstraint('ev-store', MY_VERSION);

var hashKey = '__EV_STORE_KEY@' + MY_VERSION;

var index$14 = EvStore$1;

function EvStore$1(elem) {
    var hash = elem[hashKey];

    if (!hash) {
        hash = elem[hashKey] = {};
    }

    return hash;
}

var EvStore = index$14;

var evHook$1 = EvHook;

function EvHook(value) {
    if (!(this instanceof EvHook)) {
        return new EvHook(value);
    }

    this.value = value;
}

EvHook.prototype.hook = function (node, propertyName) {
    var es = EvStore(node);
    var propName = propertyName.substr(3);

    es[propName] = this.value;
};

EvHook.prototype.unhook = function(node, propertyName) {
    var es = EvStore(node);
    var propName = propertyName.substr(3);

    es[propName] = undefined;
};

var isArray$4 = index$2;

var VNode$1 = vnode;
var VText$1 = vtext;
var isVNode$3 = isVnode;
var isVText$3 = isVtext;
var isWidget$6 = isWidget_1;
var isHook$3 = isVhook;
var isVThunk = isThunk_1;

var parseTag = parseTag_1;
var softSetHook = softSetHook$1;
var evHook = evHook$1;

var index$10 = h$2;

function h$2(tagName, properties, children) {
    var childNodes = [];
    var tag, props, key, namespace;

    if (!children && isChildren(properties)) {
        children = properties;
        props = {};
    }

    props = props || properties || {};
    tag = parseTag(tagName, props);

    // support keys
    if (props.hasOwnProperty('key')) {
        key = props.key;
        props.key = undefined;
    }

    // support namespace
    if (props.hasOwnProperty('namespace')) {
        namespace = props.namespace;
        props.namespace = undefined;
    }

    // fix cursor bug
    if (tag === 'INPUT' &&
        !namespace &&
        props.hasOwnProperty('value') &&
        props.value !== undefined &&
        !isHook$3(props.value)
    ) {
        props.value = softSetHook(props.value);
    }

    transformProperties(props);

    if (children !== undefined && children !== null) {
        addChild(children, childNodes, tag, props);
    }


    return new VNode$1(tag, props, childNodes, key, namespace);
}

function addChild(c, childNodes, tag, props) {
    if (typeof c === 'string') {
        childNodes.push(new VText$1(c));
    } else if (typeof c === 'number') {
        childNodes.push(new VText$1(String(c)));
    } else if (isChild(c)) {
        childNodes.push(c);
    } else if (isArray$4(c)) {
        for (var i = 0; i < c.length; i++) {
            addChild(c[i], childNodes, tag, props);
        }
    } else if (c === null || c === undefined) {
        return;
    } else {
        throw UnexpectedVirtualElement({
            foreignObject: c,
            parentVnode: {
                tagName: tag,
                properties: props
            }
        });
    }
}

function transformProperties(props) {
    for (var propName in props) {
        if (props.hasOwnProperty(propName)) {
            var value = props[propName];

            if (isHook$3(value)) {
                continue;
            }

            if (propName.substr(0, 3) === 'ev-') {
                // add ev-foo support
                props[propName] = evHook(value);
            }
        }
    }
}

function isChild(x) {
    return isVNode$3(x) || isVText$3(x) || isWidget$6(x) || isVThunk(x);
}

function isChildren(x) {
    return typeof x === 'string' || isArray$4(x) || isChild(x);
}

function UnexpectedVirtualElement(data) {
    var err = new Error();

    err.type = 'virtual-hyperscript.unexpected.virtual-element';
    err.message = 'Unexpected virtual child passed to h().\n' +
        'Expected a VNode / Vthunk / VWidget / string but:\n' +
        'got:\n' +
        errorString(data.foreignObject) +
        '.\n' +
        'The parent vnode is:\n' +
        errorString(data.parentVnode);
        '\n' +
        'Suggested fix: change your `h(..., [ ... ])` callsite.';
    err.foreignObject = data.foreignObject;
    err.parentVnode = data.parentVnode;

    return err;
}

function errorString(obj) {
    try {
        return JSON.stringify(obj, null, '    ');
    } catch (e) {
        return String(obj);
    }
}

var h$1 = index$10;

var h_1 = h$1;

var createElement$2 = createElement_1;

var createElement_1$2 = createElement$2;

var diff = diff_1;
var patch = patch_1;
var h = h_1;
var create$5 = createElement_1$2;
var VNode = vnode;
var VText = vtext;

var index = {
    diff: diff,
    patch: patch,
    h: h,
    create: create$5,
    VNode: VNode,
    VText: VText
};

var h = index.h;
var create$5 = index.create;
var diff = index.diff;
var patch = index.patch;

var _createClass$10 = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck$10(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function createTree(handler, tag, attributes, children) {
    var toAttrs = function toAttrs(attrs) {
        var elAttributes = function (_arg2) {
            return _arg2.tail == null ? null : ["attributes", createObj(_arg2)];
        }(choose$$1(function (x) {
            return x;
        }, map$$1(function (_arg1) {
            return _arg1.Case === "Attribute" ? function () {
                var v = _arg1.Fields[0][1];
                var k = _arg1.Fields[0][0];
                return [k, v];
            }() : null;
        }, attrs)));

        var props = map$$1(function (_arg4) {
            return _arg4.Case === "Style" ? ["style", createObj(_arg4.Fields[0])] : _arg4.Case === "Property" ? function () {
                var v = _arg4.Fields[0][1];
                var k = _arg4.Fields[0][0];
                return [k, v];
            }() : _arg4.Case === "EventHandler" ? function () {
                var f = _arg4.Fields[0][1];
                var ev = _arg4.Fields[0][0];
                return [ev, function ($var13) {
                    return handler(f($var13));
                }];
            }() : function () {
                throw new Error("Shouldn't happen");
            }();
        }, filter$$1(function (_arg3) {
            return _arg3.Case === "Attribute" ? false : true;
        }, attrs));
        return createObj(elAttributes != null ? new List$1(elAttributes, props) : props);
    };

    var elem = h(tag, toAttrs(attributes), Array.from(children));
    return elem;
}
var RenderState = function () {
    function RenderState(caseName, fields) {
        _classCallCheck$10(this, RenderState);

        this.Case = caseName;
        this.Fields = fields;
    }

    _createClass$10(RenderState, [{
        key: _Symbol.reflection,
        value: function () {
            return {
                type: "Fable.Arch.Virtualdom.RenderState",
                interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
                cases: {
                    ExtraRequest: [],
                    NoRequest: [],
                    PendingRequest: []
                }
            };
        }
    }, {
        key: "Equals",
        value: function (other) {
            return equalsUnions(this, other);
        }
    }, {
        key: "CompareTo",
        value: function (other) {
            return compareUnions(this, other);
        }
    }]);

    return RenderState;
}();
setType("Fable.Arch.Virtualdom.RenderState", RenderState);
var ViewState = function () {
    function ViewState(currentTree, nextTree, node, renderState) {
        _classCallCheck$10(this, ViewState);

        this.CurrentTree = currentTree;
        this.NextTree = nextTree;
        this.Node = node;
        this.RenderState = renderState;
    }

    _createClass$10(ViewState, [{
        key: _Symbol.reflection,
        value: function () {
            return {
                type: "Fable.Arch.Virtualdom.ViewState",
                interfaces: ["FSharpRecord", "System.IEquatable"],
                properties: {
                    CurrentTree: Any,
                    NextTree: Any,
                    Node: Interface("Fable.Import.Browser.Node"),
                    RenderState: RenderState
                }
            };
        }
    }, {
        key: "Equals",
        value: function (other) {
            return equalsRecords(this, other);
        }
    }]);

    return ViewState;
}();
setType("Fable.Arch.Virtualdom.ViewState", ViewState);
function renderSomething(handler, node) {
    var _target0 = function _target0(attrs, nodes, tag) {
        return createTree(handler, tag, attrs, map$$1(function (node_1) {
            return renderSomething(handler, node_1);
        }, nodes));
    };

    if (node.Case === "Svg") {
        return _target0(node.Fields[0][1], node.Fields[1], node.Fields[0][0]);
    } else {
        if (node.Case === "VoidElement") {
            var tag = node.Fields[0][0];
            var attrs = node.Fields[0][1];
            return createTree(handler, tag, attrs, new List$1());
        } else {
            if (node.Case === "Text") {
                return node.Fields[0];
            } else {
                if (node.Case === "WhiteSpace") {
                    return node.Fields[0];
                } else {
                    return _target0(node.Fields[0][1], node.Fields[1], node.Fields[0][0]);
                }
            }
        }
    }
}
function render(handler, view, viewState) {
    var tree = renderSomething(handler, view);
    return new ViewState(viewState.CurrentTree, tree, viewState.Node, viewState.RenderState);
}
function createRender(selector, handler, view) {
    var node = document.body.querySelector(selector);
    var tree = renderSomething(handler, view);
    var vdomNode = create$5(tree);
    node.appendChild(vdomNode);
    var viewState = new ViewState(tree, tree, vdomNode, new RenderState("NoRequest", []));

    var raf = function raf(cb) {
        return window.requestAnimationFrame(function (fb) {
            cb();
        });
    };

    var render_ = function render_(handler_1) {
        return function (view_1) {
            var viewState_ = render(handler_1, view_1, viewState);
            viewState = viewState_;

            var callBack = function callBack() {
                var matchValue = viewState.RenderState;

                if (matchValue.Case === "ExtraRequest") {
                    {
                        var RenderState_1 = new RenderState("NoRequest", []);
                        viewState = new ViewState(viewState.CurrentTree, viewState.NextTree, viewState.Node, RenderState_1);
                    }
                } else {
                    if (matchValue.Case === "NoRequest") {
                        throw new Error("Shouldn't happen");
                    } else {
                        raf(callBack);
                        {
                            var _RenderState_ = new RenderState("ExtraRequest", []);

                            viewState = new ViewState(viewState.CurrentTree, viewState.NextTree, viewState.Node, _RenderState_);
                        }
                        var patches = diff(viewState.CurrentTree, viewState.NextTree);
                        patch(viewState.Node, patches);
                        viewState = new ViewState(viewState.NextTree, viewState.NextTree, viewState.Node, viewState.RenderState);
                    }
                }
            };

            {
                var matchValue = viewState.RenderState;

                if (matchValue.Case === "NoRequest") {
                    raf(callBack);
                }
            }
            {
                var RenderState_1 = new RenderState("PendingRequest", []);
                viewState = new ViewState(viewState.CurrentTree, viewState.NextTree, viewState.Node, RenderState_1);
            }
        };
    };

    return render_;
}

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var Details = function () {
    function Details(caseName, fields) {
        _classCallCheck(this, Details);

        this.Case = caseName;
        this.Fields = fields;
    }

    _createClass(Details, [{
        key: _Symbol.reflection,
        value: function () {
            return {
                type: "Main.Details",
                interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
                cases: {
                    Character: ["string"],
                    Film: ["string", "string"]
                }
            };
        }
    }, {
        key: "Equals",
        value: function (other) {
            return equalsUnions(this, other);
        }
    }, {
        key: "CompareTo",
        value: function (other) {
            return compareUnions(this, other);
        }
    }]);

    return Details;
}();
setType("Main.Details", Details);
var Entity = function () {
    function Entity(related, details) {
        _classCallCheck(this, Entity);

        this.related = related;
        this.details = details;
    }

    _createClass(Entity, [{
        key: _Symbol.reflection,
        value: function () {
            return {
                type: "Main.Entity",
                interfaces: ["FSharpRecord", "System.IEquatable", "System.IComparable"],
                properties: {
                    related: makeGeneric(List$1, {
                        T: "string"
                    }),
                    details: Details
                }
            };
        }
    }, {
        key: "Equals",
        value: function (other) {
            return equalsRecords(this, other);
        }
    }, {
        key: "CompareTo",
        value: function (other) {
            return compareRecords(this, other);
        }
    }]);

    return Entity;
}();
setType("Main.Entity", Entity);
var Model = function () {
    function Model(caseName, fields) {
        _classCallCheck(this, Model);

        this.Case = caseName;
        this.Fields = fields;
    }

    _createClass(Model, [{
        key: _Symbol.reflection,
        value: function () {
            return {
                type: "Main.Model",
                interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
                cases: {
                    ErrorScreen: [],
                    InitialScreen: [],
                    List: [Entity, makeGeneric(List$1, {
                        T: Entity
                    })],
                    Loading: [Entity]
                }
            };
        }
    }, {
        key: "Equals",
        value: function (other) {
            return equalsUnions(this, other);
        }
    }, {
        key: "CompareTo",
        value: function (other) {
            return compareUnions(this, other);
        }
    }]);

    return Model;
}();
setType("Main.Model", Model);
var ResponseJson = function () {
    function ResponseJson(name, title, episode_id, characters, films) {
        _classCallCheck(this, ResponseJson);

        this.name = name;
        this.title = title;
        this.episode_id = episode_id;
        this.characters = characters;
        this.films = films;
    }

    _createClass(ResponseJson, [{
        key: _Symbol.reflection,
        value: function () {
            return {
                type: "Main.ResponseJson",
                interfaces: ["FSharpRecord", "System.IEquatable", "System.IComparable"],
                properties: {
                    name: "string",
                    title: "string",
                    episode_id: "string",
                    characters: makeGeneric(List$1, {
                        T: "string"
                    }),
                    films: makeGeneric(List$1, {
                        T: "string"
                    })
                }
            };
        }
    }, {
        key: "Equals",
        value: function (other) {
            return equalsRecords(this, other);
        }
    }, {
        key: "CompareTo",
        value: function (other) {
            return compareRecords(this, other);
        }
    }]);

    return ResponseJson;
}();
setType("Main.ResponseJson", ResponseJson);
function parse(json) {
    var obj = ofJson(json, {
        T: ResponseJson
    });

    if (isNullOrEmpty(obj.name)) {
        return new Entity(obj.characters, new Details("Film", [obj.title, obj.episode_id]));
    } else {
        return new Entity(obj.films, new Details("Character", [obj.name]));
    }
}
var Msg = function () {
    function Msg(caseName, fields) {
        _classCallCheck(this, Msg);

        this.Case = caseName;
        this.Fields = fields;
    }

    _createClass(Msg, [{
        key: _Symbol.reflection,
        value: function () {
            return {
                type: "Main.Msg",
                interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
                cases: {
                    FetchFail: [],
                    Load: [Entity],
                    ToList: [Entity, makeGeneric(List$1, {
                        T: Entity
                    })]
                }
            };
        }
    }, {
        key: "Equals",
        value: function (other) {
            return equalsUnions(this, other);
        }
    }, {
        key: "CompareTo",
        value: function (other) {
            return compareUnions(this, other);
        }
    }]);

    return Msg;
}();
setType("Main.Msg", Msg);
function fetchEntity(url) {
    return function (builder_) {
        return builder_.Delay(function () {
            var urlWithoutProtocol = replace$$1(url, "http://", "//");
            return _fetch(urlWithoutProtocol, {}).then(function (_arg1) {
                return _arg1.text().then(function (_arg2) {
                    return Promise.resolve(parse(_arg2));
                });
            });
        });
    }(PromiseImpl.promise);
}
function getFirstCharacter(handler) {
    (function (pr) {
        return pr.then(handler);
    })(function (builder_) {
        return builder_.Delay(function () {
            return fetchEntity("http://swapi.co/api/people/2/").then(function (_arg1) {
                return Promise.resolve(new Msg("Load", [_arg1]));
            });
        });
    }(PromiseImpl.promise));
}
function getRelatedEntities(entity, handler) {
    (function (pr) {
        return pr.then(handler);
    })(Promise.all(map$$1(function (url) {
        return fetchEntity(url);
    }, entity.related)).then(function (list) {
        return new Msg("ToList", [entity, ofArray(list)]);
    }));
}
function update(model, msg) {
    return msg.Case === "ToList" ? [new Model("List", [msg.Fields[0], msg.Fields[1]]), new List$1()] : msg.Case === "FetchFail" ? [new Model("ErrorScreen", []), new List$1()] : [new Model("Loading", [msg.Fields[0]]), ofArray([function (handler) {
        getRelatedEntities(msg.Fields[0], handler);
    }])];
}
function bgColor(entity) {
    return entity.details.Case === "Film" ? "rgba(52, 152, 219,1.0)" : "rgba(230, 126, 34,1.0)";
}
function mainStyle(entity) {
    return new Types.Attribute("Style", [ofArray([["background-color", bgColor(entity)], ["width", "200px"], ["height", "200px"], ["color", "white"], ["font-family", "-apple-system, system, sans-serif"], ["margin", "20px 0px 0px 20px"], ["cursor", "pointer"]])]);
}
function filmNumberStyle() {
    return new Types.Attribute("Style", [ofArray([["padding", "20px 20px 0px 20px"], ["font-size", "60px"]])]);
}
function captionStyle() {
    return new Types.Attribute("Style", [ofArray([["padding", "20px"], ["font-size", "18px"]])]);
}
function filmContents(title, episode) {
    return ofArray([function () {
        var tagName = "div";
        return function (children) {
            return new Types.DomNode("Element", [[tagName, ofArray([filmNumberStyle()])], children]);
        };
    }()(ofArray([new Types.DomNode("Text", [episode])])), function () {
        var tagName = "div";
        return function (children) {
            return new Types.DomNode("Element", [[tagName, ofArray([captionStyle()])], children]);
        };
    }()(ofArray([new Types.DomNode("Text", [title])]))]);
}
function characterContents(name) {
    return ofArray([function () {
        var tagName = "div";
        return function (children) {
            return new Types.DomNode("Element", [[tagName, ofArray([captionStyle()])], children]);
        };
    }()(ofArray([new Types.DomNode("Text", [name])]))]);
}
function entityView(entity) {
    var attributes = ofArray([mainStyle(entity), function () {
        var h = function h(e) {
            e.stopPropagation();
            e.preventDefault();
            return function (_arg1) {
                return entity;
            }(e);
        };

        return new Types.Attribute("EventHandler", [["onclick", h]]);
    }()]);
    var contents = entity.details.Case === "Character" ? characterContents(entity.details.Fields[0]) : filmContents(entity.details.Fields[0], entity.details.Fields[1]);
    return function () {
        var tagName = "div";
        return function (children) {
            return new Types.DomNode("Element", [[tagName, attributes], children]);
        };
    }()(contents);
}
function messageStyle() {
    return new Types.Attribute("Style", [ofArray([["margin", "20px 0px 0px 20px"], ["width", "200px"], ["height", "200px"], ["font-family", "-apple-system, system, sans-serif"], ["color", "rgba(149, 165, 166,1.0)"], ["font-size", "18px"]])]);
}
function messageView(t) {
    return function () {
        var tagName = "div";
        return function (children) {
            return new Types.DomNode("Element", [[tagName, ofArray([messageStyle()])], children]);
        };
    }()(ofArray([new Types.DomNode("Text", [t])]));
}
function loadingMessageView(entity) {
    return entity.details.Case === "Character" ? messageView("Loading " + entity.details.Fields[0] + " films...") : messageView("Loading " + entity.details.Fields[0] + " characters...");
}
function mappedEntityView(entity) {
    return map$5(function (arg0) {
        return new Msg("Load", [arg0]);
    }, entityView(entity));
}
function view(model) {
    return model.Case === "Loading" ? function () {
        var tagName = "div";
        return function (children) {
            return new Types.DomNode("Element", [[tagName, ofArray([new Types.Attribute("Style", [ofArray([["display", "flex"]])])])], children]);
        };
    }()(ofArray([mappedEntityView(model.Fields[0]), loadingMessageView(model.Fields[0])])) : model.Case === "List" ? function () {
        var listView = map$$1(function (entity) {
            return mappedEntityView(entity);
        }, model.Fields[1]);
        return function () {
            var tagName = "div";
            return function (children) {
                return new Types.DomNode("Element", [[tagName, ofArray([new Types.Attribute("Style", [ofArray([["display", "flex"]])])])], children]);
            };
        }()(ofArray([mappedEntityView(model.Fields[0]), function () {
            var tagName = "div";
            return function (children) {
                return new Types.DomNode("Element", [[tagName, new List$1()], children]);
            };
        }()(listView)]));
    }() : model.Case === "ErrorScreen" ? messageView("An error ocurred. Please refresh the page and try again - and may the Force be with you!") : messageView("Loading amazing characters and films...");
}
AppApi.start(AppApi.withSubscriber(function (x) {
    console.log("Event received: ", x);
}, AppApi.withInitMessage(function (handler) {
    getFirstCharacter(handler);
}, AppApi.withStartNodeSelector("#app", AppApi.createApp(new Model("InitialScreen", []), function (model) {
    return view(model);
}, function (model) {
    return function (msg) {
        return update(model, msg);
    };
}, function (selector) {
    return function (handler) {
        return function (view_1) {
            return createRender(selector, handler, view_1);
        };
    };
})))));

exports.Details = Details;
exports.Entity = Entity;
exports.Model = Model;
exports.ResponseJson = ResponseJson;
exports.parse = parse;
exports.Msg = Msg;
exports.fetchEntity = fetchEntity;
exports.getFirstCharacter = getFirstCharacter;
exports.getRelatedEntities = getRelatedEntities;
exports.update = update;
exports.bgColor = bgColor;
exports.mainStyle = mainStyle;
exports.filmNumberStyle = filmNumberStyle;
exports.captionStyle = captionStyle;
exports.filmContents = filmContents;
exports.characterContents = characterContents;
exports.entityView = entityView;
exports.messageStyle = messageStyle;
exports.messageView = messageView;
exports.loadingMessageView = loadingMessageView;
exports.mappedEntityView = mappedEntityView;
exports.view = view;

}((this.Main = this.Main || {})));

//# sourceMappingURL=bundle.js.map