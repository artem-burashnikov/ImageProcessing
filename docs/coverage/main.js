/* Chartist.js 0.11.0
 * Copyright © 2017 Gion Kunz
 * Free to use under either the WTFPL license or the MIT license.
 * https://raw.githubusercontent.com/gionkunz/chartist-js/master/LICENSE-WTFPL
 * https://raw.githubusercontent.com/gionkunz/chartist-js/master/LICENSE-MIT
 */

!function (a, b) { "function" == typeof define && define.amd ? define("Chartist", [], function () { return a.Chartist = b() }) : "object" == typeof module && module.exports ? module.exports = b() : a.Chartist = b() }(this, function () {
    var a = { version: "0.11.0" }; return function (a, b, c) { "use strict"; c.namespaces = { svg: "http://www.w3.org/2000/svg", xmlns: "http://www.w3.org/2000/xmlns/", xhtml: "http://www.w3.org/1999/xhtml", xlink: "http://www.w3.org/1999/xlink", ct: "http://gionkunz.github.com/chartist-js/ct" }, c.noop = function (a) { return a }, c.alphaNumerate = function (a) { return String.fromCharCode(97 + a % 26) }, c.extend = function (a) { var b, d, e; for (a = a || {}, b = 1; b < arguments.length; b++) { d = arguments[b]; for (var f in d) e = d[f], "object" != typeof e || null === e || e instanceof Array ? a[f] = e : a[f] = c.extend(a[f], e) } return a }, c.replaceAll = function (a, b, c) { return a.replace(new RegExp(b, "g"), c) }, c.ensureUnit = function (a, b) { return "number" == typeof a && (a += b), a }, c.quantity = function (a) { if ("string" == typeof a) { var b = /^(\d+)\s*(.*)$/g.exec(a); return { value: +b[1], unit: b[2] || void 0 } } return { value: a } }, c.querySelector = function (a) { return a instanceof Node ? a : b.querySelector(a) }, c.times = function (a) { return Array.apply(null, new Array(a)) }, c.sum = function (a, b) { return a + (b ? b : 0) }, c.mapMultiply = function (a) { return function (b) { return b * a } }, c.mapAdd = function (a) { return function (b) { return b + a } }, c.serialMap = function (a, b) { var d = [], e = Math.max.apply(null, a.map(function (a) { return a.length })); return c.times(e).forEach(function (c, e) { var f = a.map(function (a) { return a[e] }); d[e] = b.apply(null, f) }), d }, c.roundWithPrecision = function (a, b) { var d = Math.pow(10, b || c.precision); return Math.round(a * d) / d }, c.precision = 8, c.escapingMap = { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#039;" }, c.serialize = function (a) { return null === a || void 0 === a ? a : ("number" == typeof a ? a = "" + a : "object" == typeof a && (a = JSON.stringify({ data: a })), Object.keys(c.escapingMap).reduce(function (a, b) { return c.replaceAll(a, b, c.escapingMap[b]) }, a)) }, c.deserialize = function (a) { if ("string" != typeof a) return a; a = Object.keys(c.escapingMap).reduce(function (a, b) { return c.replaceAll(a, c.escapingMap[b], b) }, a); try { a = JSON.parse(a), a = void 0 !== a.data ? a.data : a } catch (b) { } return a }, c.createSvg = function (a, b, d, e) { var f; return b = b || "100%", d = d || "100%", Array.prototype.slice.call(a.querySelectorAll("svg")).filter(function (a) { return a.getAttributeNS(c.namespaces.xmlns, "ct") }).forEach(function (b) { a.removeChild(b) }), f = new c.Svg("svg").attr({ width: b, height: d }).addClass(e), f._node.style.width = b, f._node.style.height = d, a.appendChild(f._node), f }, c.normalizeData = function (a, b, d) { var e, f = { raw: a, normalized: {} }; return f.normalized.series = c.getDataArray({ series: a.series || [] }, b, d), e = f.normalized.series.every(function (a) { return a instanceof Array }) ? Math.max.apply(null, f.normalized.series.map(function (a) { return a.length })) : f.normalized.series.length, f.normalized.labels = (a.labels || []).slice(), Array.prototype.push.apply(f.normalized.labels, c.times(Math.max(0, e - f.normalized.labels.length)).map(function () { return "" })), b && c.reverseData(f.normalized), f }, c.safeHasProperty = function (a, b) { return null !== a && "object" == typeof a && a.hasOwnProperty(b) }, c.isDataHoleValue = function (a) { return null === a || void 0 === a || "number" == typeof a && isNaN(a) }, c.reverseData = function (a) { a.labels.reverse(), a.series.reverse(); for (var b = 0; b < a.series.length; b++)"object" == typeof a.series[b] && void 0 !== a.series[b].data ? a.series[b].data.reverse() : a.series[b] instanceof Array && a.series[b].reverse() }, c.getDataArray = function (a, b, d) { function e(a) { if (c.safeHasProperty(a, "value")) return e(a.value); if (c.safeHasProperty(a, "data")) return e(a.data); if (a instanceof Array) return a.map(e); if (!c.isDataHoleValue(a)) { if (d) { var b = {}; return "string" == typeof d ? b[d] = c.getNumberOrUndefined(a) : b.y = c.getNumberOrUndefined(a), b.x = a.hasOwnProperty("x") ? c.getNumberOrUndefined(a.x) : b.x, b.y = a.hasOwnProperty("y") ? c.getNumberOrUndefined(a.y) : b.y, b } return c.getNumberOrUndefined(a) } } return a.series.map(e) }, c.normalizePadding = function (a, b) { return b = b || 0, "number" == typeof a ? { top: a, right: a, bottom: a, left: a } : { top: "number" == typeof a.top ? a.top : b, right: "number" == typeof a.right ? a.right : b, bottom: "number" == typeof a.bottom ? a.bottom : b, left: "number" == typeof a.left ? a.left : b } }, c.getMetaData = function (a, b) { var c = a.data ? a.data[b] : a[b]; return c ? c.meta : void 0 }, c.orderOfMagnitude = function (a) { return Math.floor(Math.log(Math.abs(a)) / Math.LN10) }, c.projectLength = function (a, b, c) { return b / c.range * a }, c.getAvailableHeight = function (a, b) { return Math.max((c.quantity(b.height).value || a.height()) - (b.chartPadding.top + b.chartPadding.bottom) - b.axisX.offset, 0) }, c.getHighLow = function (a, b, d) { function e(a) { if (void 0 !== a) if (a instanceof Array) for (var b = 0; b < a.length; b++)e(a[b]); else { var c = d ? +a[d] : +a; g && c > f.high && (f.high = c), h && c < f.low && (f.low = c) } } b = c.extend({}, b, d ? b["axis" + d.toUpperCase()] : {}); var f = { high: void 0 === b.high ? -Number.MAX_VALUE : +b.high, low: void 0 === b.low ? Number.MAX_VALUE : +b.low }, g = void 0 === b.high, h = void 0 === b.low; return (g || h) && e(a), (b.referenceValue || 0 === b.referenceValue) && (f.high = Math.max(b.referenceValue, f.high), f.low = Math.min(b.referenceValue, f.low)), f.high <= f.low && (0 === f.low ? f.high = 1 : f.low < 0 ? f.high = 0 : f.high > 0 ? f.low = 0 : (f.high = 1, f.low = 0)), f }, c.isNumeric = function (a) { return null !== a && isFinite(a) }, c.isFalseyButZero = function (a) { return !a && 0 !== a }, c.getNumberOrUndefined = function (a) { return c.isNumeric(a) ? +a : void 0 }, c.isMultiValue = function (a) { return "object" == typeof a && ("x" in a || "y" in a) }, c.getMultiValue = function (a, b) { return c.isMultiValue(a) ? c.getNumberOrUndefined(a[b || "y"]) : c.getNumberOrUndefined(a) }, c.rho = function (a) { function b(a, c) { return a % c === 0 ? c : b(c, a % c) } function c(a) { return a * a + 1 } if (1 === a) return a; var d, e = 2, f = 2; if (a % 2 === 0) return 2; do e = c(e) % a, f = c(c(f)) % a, d = b(Math.abs(e - f), a); while (1 === d); return d }, c.getBounds = function (a, b, d, e) { function f(a, b) { return a === (a += b) && (a *= 1 + (b > 0 ? o : -o)), a } var g, h, i, j = 0, k = { high: b.high, low: b.low }; k.valueRange = k.high - k.low, k.oom = c.orderOfMagnitude(k.valueRange), k.step = Math.pow(10, k.oom), k.min = Math.floor(k.low / k.step) * k.step, k.max = Math.ceil(k.high / k.step) * k.step, k.range = k.max - k.min, k.numberOfSteps = Math.round(k.range / k.step); var l = c.projectLength(a, k.step, k), m = l < d, n = e ? c.rho(k.range) : 0; if (e && c.projectLength(a, 1, k) >= d) k.step = 1; else if (e && n < k.step && c.projectLength(a, n, k) >= d) k.step = n; else for (; ;) { if (m && c.projectLength(a, k.step, k) <= d) k.step *= 2; else { if (m || !(c.projectLength(a, k.step / 2, k) >= d)) break; if (k.step /= 2, e && k.step % 1 !== 0) { k.step *= 2; break } } if (j++ > 1e3) throw new Error("Exceeded maximum number of iterations while optimizing scale step!") } var o = 2.221e-16; for (k.step = Math.max(k.step, o), h = k.min, i = k.max; h + k.step <= k.low;)h = f(h, k.step); for (; i - k.step >= k.high;)i = f(i, -k.step); k.min = h, k.max = i, k.range = k.max - k.min; var p = []; for (g = k.min; g <= k.max; g = f(g, k.step)) { var q = c.roundWithPrecision(g); q !== p[p.length - 1] && p.push(q) } return k.values = p, k }, c.polarToCartesian = function (a, b, c, d) { var e = (d - 90) * Math.PI / 180; return { x: a + c * Math.cos(e), y: b + c * Math.sin(e) } }, c.createChartRect = function (a, b, d) { var e = !(!b.axisX && !b.axisY), f = e ? b.axisY.offset : 0, g = e ? b.axisX.offset : 0, h = a.width() || c.quantity(b.width).value || 0, i = a.height() || c.quantity(b.height).value || 0, j = c.normalizePadding(b.chartPadding, d); h = Math.max(h, f + j.left + j.right), i = Math.max(i, g + j.top + j.bottom); var k = { padding: j, width: function () { return this.x2 - this.x1 }, height: function () { return this.y1 - this.y2 } }; return e ? ("start" === b.axisX.position ? (k.y2 = j.top + g, k.y1 = Math.max(i - j.bottom, k.y2 + 1)) : (k.y2 = j.top, k.y1 = Math.max(i - j.bottom - g, k.y2 + 1)), "start" === b.axisY.position ? (k.x1 = j.left + f, k.x2 = Math.max(h - j.right, k.x1 + 1)) : (k.x1 = j.left, k.x2 = Math.max(h - j.right - f, k.x1 + 1))) : (k.x1 = j.left, k.x2 = Math.max(h - j.right, k.x1 + 1), k.y2 = j.top, k.y1 = Math.max(i - j.bottom, k.y2 + 1)), k }, c.createGrid = function (a, b, d, e, f, g, h, i) { var j = {}; j[d.units.pos + "1"] = a, j[d.units.pos + "2"] = a, j[d.counterUnits.pos + "1"] = e, j[d.counterUnits.pos + "2"] = e + f; var k = g.elem("line", j, h.join(" ")); i.emit("draw", c.extend({ type: "grid", axis: d, index: b, group: g, element: k }, j)) }, c.createGridBackground = function (a, b, c, d) { var e = a.elem("rect", { x: b.x1, y: b.y2, width: b.width(), height: b.height() }, c, !0); d.emit("draw", { type: "gridBackground", group: a, element: e }) }, c.createLabel = function (a, d, e, f, g, h, i, j, k, l, m) { var n, o = {}; if (o[g.units.pos] = a + i[g.units.pos], o[g.counterUnits.pos] = i[g.counterUnits.pos], o[g.units.len] = d, o[g.counterUnits.len] = Math.max(0, h - 10), l) { var p = b.createElement("span"); p.className = k.join(" "), p.setAttribute("xmlns", c.namespaces.xhtml), p.innerText = f[e], p.style[g.units.len] = Math.round(o[g.units.len]) + "px", p.style[g.counterUnits.len] = Math.round(o[g.counterUnits.len]) + "px", n = j.foreignObject(p, c.extend({ style: "overflow: visible;" }, o)) } else n = j.elem("text", o, k.join(" ")).text(f[e]); m.emit("draw", c.extend({ type: "label", axis: g, index: e, group: j, element: n, text: f[e] }, o)) }, c.getSeriesOption = function (a, b, c) { if (a.name && b.series && b.series[a.name]) { var d = b.series[a.name]; return d.hasOwnProperty(c) ? d[c] : b[c] } return b[c] }, c.optionsProvider = function (b, d, e) { function f(b) { var f = h; if (h = c.extend({}, j), d) for (i = 0; i < d.length; i++) { var g = a.matchMedia(d[i][0]); g.matches && (h = c.extend(h, d[i][1])) } e && b && e.emit("optionsChanged", { previousOptions: f, currentOptions: h }) } function g() { k.forEach(function (a) { a.removeListener(f) }) } var h, i, j = c.extend({}, b), k = []; if (!a.matchMedia) throw "window.matchMedia not found! Make sure you're using a polyfill."; if (d) for (i = 0; i < d.length; i++) { var l = a.matchMedia(d[i][0]); l.addListener(f), k.push(l) } return f(), { removeMediaQueryListeners: g, getCurrentOptions: function () { return c.extend({}, h) } } }, c.splitIntoSegments = function (a, b, d) { var e = { increasingX: !1, fillHoles: !1 }; d = c.extend({}, e, d); for (var f = [], g = !0, h = 0; h < a.length; h += 2)void 0 === c.getMultiValue(b[h / 2].value) ? d.fillHoles || (g = !0) : (d.increasingX && h >= 2 && a[h] <= a[h - 2] && (g = !0), g && (f.push({ pathCoordinates: [], valueData: [] }), g = !1), f[f.length - 1].pathCoordinates.push(a[h], a[h + 1]), f[f.length - 1].valueData.push(b[h / 2])); return f } }(window, document, a), function (a, b, c) { "use strict"; c.Interpolation = {}, c.Interpolation.none = function (a) { var b = { fillHoles: !1 }; return a = c.extend({}, b, a), function (b, d) { for (var e = new c.Svg.Path, f = !0, g = 0; g < b.length; g += 2) { var h = b[g], i = b[g + 1], j = d[g / 2]; void 0 !== c.getMultiValue(j.value) ? (f ? e.move(h, i, !1, j) : e.line(h, i, !1, j), f = !1) : a.fillHoles || (f = !0) } return e } }, c.Interpolation.simple = function (a) { var b = { divisor: 2, fillHoles: !1 }; a = c.extend({}, b, a); var d = 1 / Math.max(1, a.divisor); return function (b, e) { for (var f, g, h, i = new c.Svg.Path, j = 0; j < b.length; j += 2) { var k = b[j], l = b[j + 1], m = (k - f) * d, n = e[j / 2]; void 0 !== n.value ? (void 0 === h ? i.move(k, l, !1, n) : i.curve(f + m, g, k - m, l, k, l, !1, n), f = k, g = l, h = n) : a.fillHoles || (f = k = h = void 0) } return i } }, c.Interpolation.cardinal = function (a) { var b = { tension: 1, fillHoles: !1 }; a = c.extend({}, b, a); var d = Math.min(1, Math.max(0, a.tension)), e = 1 - d; return function f(b, g) { var h = c.splitIntoSegments(b, g, { fillHoles: a.fillHoles }); if (h.length) { if (h.length > 1) { var i = []; return h.forEach(function (a) { i.push(f(a.pathCoordinates, a.valueData)) }), c.Svg.Path.join(i) } if (b = h[0].pathCoordinates, g = h[0].valueData, b.length <= 4) return c.Interpolation.none()(b, g); for (var j, k = (new c.Svg.Path).move(b[0], b[1], !1, g[0]), l = 0, m = b.length; m - 2 * !j > l; l += 2) { var n = [{ x: +b[l - 2], y: +b[l - 1] }, { x: +b[l], y: +b[l + 1] }, { x: +b[l + 2], y: +b[l + 3] }, { x: +b[l + 4], y: +b[l + 5] }]; j ? l ? m - 4 === l ? n[3] = { x: +b[0], y: +b[1] } : m - 2 === l && (n[2] = { x: +b[0], y: +b[1] }, n[3] = { x: +b[2], y: +b[3] }) : n[0] = { x: +b[m - 2], y: +b[m - 1] } : m - 4 === l ? n[3] = n[2] : l || (n[0] = { x: +b[l], y: +b[l + 1] }), k.curve(d * (-n[0].x + 6 * n[1].x + n[2].x) / 6 + e * n[2].x, d * (-n[0].y + 6 * n[1].y + n[2].y) / 6 + e * n[2].y, d * (n[1].x + 6 * n[2].x - n[3].x) / 6 + e * n[2].x, d * (n[1].y + 6 * n[2].y - n[3].y) / 6 + e * n[2].y, n[2].x, n[2].y, !1, g[(l + 2) / 2]) } return k } return c.Interpolation.none()([]) } }, c.Interpolation.monotoneCubic = function (a) { var b = { fillHoles: !1 }; return a = c.extend({}, b, a), function d(b, e) { var f = c.splitIntoSegments(b, e, { fillHoles: a.fillHoles, increasingX: !0 }); if (f.length) { if (f.length > 1) { var g = []; return f.forEach(function (a) { g.push(d(a.pathCoordinates, a.valueData)) }), c.Svg.Path.join(g) } if (b = f[0].pathCoordinates, e = f[0].valueData, b.length <= 4) return c.Interpolation.none()(b, e); var h, i, j = [], k = [], l = b.length / 2, m = [], n = [], o = [], p = []; for (h = 0; h < l; h++)j[h] = b[2 * h], k[h] = b[2 * h + 1]; for (h = 0; h < l - 1; h++)o[h] = k[h + 1] - k[h], p[h] = j[h + 1] - j[h], n[h] = o[h] / p[h]; for (m[0] = n[0], m[l - 1] = n[l - 2], h = 1; h < l - 1; h++)0 === n[h] || 0 === n[h - 1] || n[h - 1] > 0 != n[h] > 0 ? m[h] = 0 : (m[h] = 3 * (p[h - 1] + p[h]) / ((2 * p[h] + p[h - 1]) / n[h - 1] + (p[h] + 2 * p[h - 1]) / n[h]), isFinite(m[h]) || (m[h] = 0)); for (i = (new c.Svg.Path).move(j[0], k[0], !1, e[0]), h = 0; h < l - 1; h++)i.curve(j[h] + p[h] / 3, k[h] + m[h] * p[h] / 3, j[h + 1] - p[h] / 3, k[h + 1] - m[h + 1] * p[h] / 3, j[h + 1], k[h + 1], !1, e[h + 1]); return i } return c.Interpolation.none()([]) } }, c.Interpolation.step = function (a) { var b = { postpone: !0, fillHoles: !1 }; return a = c.extend({}, b, a), function (b, d) { for (var e, f, g, h = new c.Svg.Path, i = 0; i < b.length; i += 2) { var j = b[i], k = b[i + 1], l = d[i / 2]; void 0 !== l.value ? (void 0 === g ? h.move(j, k, !1, l) : (a.postpone ? h.line(j, f, !1, g) : h.line(e, k, !1, l), h.line(j, k, !1, l)), e = j, f = k, g = l) : a.fillHoles || (e = f = g = void 0) } return h } } }(window, document, a), function (a, b, c) { "use strict"; c.EventEmitter = function () { function a(a, b) { d[a] = d[a] || [], d[a].push(b) } function b(a, b) { d[a] && (b ? (d[a].splice(d[a].indexOf(b), 1), 0 === d[a].length && delete d[a]) : delete d[a]) } function c(a, b) { d[a] && d[a].forEach(function (a) { a(b) }), d["*"] && d["*"].forEach(function (c) { c(a, b) }) } var d = []; return { addEventHandler: a, removeEventHandler: b, emit: c } } }(window, document, a), function (a, b, c) { "use strict"; function d(a) { var b = []; if (a.length) for (var c = 0; c < a.length; c++)b.push(a[c]); return b } function e(a, b) { var d = b || this.prototype || c.Class, e = Object.create(d); c.Class.cloneDefinitions(e, a); var f = function () { var a, b = e.constructor || function () { }; return a = this === c ? Object.create(e) : this, b.apply(a, Array.prototype.slice.call(arguments, 0)), a }; return f.prototype = e, f["super"] = d, f.extend = this.extend, f } function f() { var a = d(arguments), b = a[0]; return a.splice(1, a.length - 1).forEach(function (a) { Object.getOwnPropertyNames(a).forEach(function (c) { delete b[c], Object.defineProperty(b, c, Object.getOwnPropertyDescriptor(a, c)) }) }), b } c.Class = { extend: e, cloneDefinitions: f } }(window, document, a), function (a, b, c) { "use strict"; function d(a, b, d) { return a && (this.data = a || {}, this.data.labels = this.data.labels || [], this.data.series = this.data.series || [], this.eventEmitter.emit("data", { type: "update", data: this.data })), b && (this.options = c.extend({}, d ? this.options : this.defaultOptions, b), this.initializeTimeoutId || (this.optionsProvider.removeMediaQueryListeners(), this.optionsProvider = c.optionsProvider(this.options, this.responsiveOptions, this.eventEmitter))), this.initializeTimeoutId || this.createChart(this.optionsProvider.getCurrentOptions()), this } function e() { return this.initializeTimeoutId ? a.clearTimeout(this.initializeTimeoutId) : (a.removeEventListener("resize", this.resizeListener), this.optionsProvider.removeMediaQueryListeners()), this } function f(a, b) { return this.eventEmitter.addEventHandler(a, b), this } function g(a, b) { return this.eventEmitter.removeEventHandler(a, b), this } function h() { a.addEventListener("resize", this.resizeListener), this.optionsProvider = c.optionsProvider(this.options, this.responsiveOptions, this.eventEmitter), this.eventEmitter.addEventHandler("optionsChanged", function () { this.update() }.bind(this)), this.options.plugins && this.options.plugins.forEach(function (a) { a instanceof Array ? a[0](this, a[1]) : a(this) }.bind(this)), this.eventEmitter.emit("data", { type: "initial", data: this.data }), this.createChart(this.optionsProvider.getCurrentOptions()), this.initializeTimeoutId = void 0 } function i(a, b, d, e, f) { this.container = c.querySelector(a), this.data = b || {}, this.data.labels = this.data.labels || [], this.data.series = this.data.series || [], this.defaultOptions = d, this.options = e, this.responsiveOptions = f, this.eventEmitter = c.EventEmitter(), this.supportsForeignObject = c.Svg.isSupported("Extensibility"), this.supportsAnimations = c.Svg.isSupported("AnimationEventsAttribute"), this.resizeListener = function () { this.update() }.bind(this), this.container && (this.container.__chartist__ && this.container.__chartist__.detach(), this.container.__chartist__ = this), this.initializeTimeoutId = setTimeout(h.bind(this), 0) } c.Base = c.Class.extend({ constructor: i, optionsProvider: void 0, container: void 0, svg: void 0, eventEmitter: void 0, createChart: function () { throw new Error("Base chart type can't be instantiated!") }, update: d, detach: e, on: f, off: g, version: c.version, supportsForeignObject: !1 }) }(window, document, a), function (a, b, c) { "use strict"; function d(a, d, e, f, g) { a instanceof Element ? this._node = a : (this._node = b.createElementNS(c.namespaces.svg, a), "svg" === a && this.attr({ "xmlns:ct": c.namespaces.ct })), d && this.attr(d), e && this.addClass(e), f && (g && f._node.firstChild ? f._node.insertBefore(this._node, f._node.firstChild) : f._node.appendChild(this._node)) } function e(a, b) { return "string" == typeof a ? b ? this._node.getAttributeNS(b, a) : this._node.getAttribute(a) : (Object.keys(a).forEach(function (b) { if (void 0 !== a[b]) if (b.indexOf(":") !== -1) { var d = b.split(":"); this._node.setAttributeNS(c.namespaces[d[0]], b, a[b]) } else this._node.setAttribute(b, a[b]) }.bind(this)), this) } function f(a, b, d, e) { return new c.Svg(a, b, d, this, e) } function g() { return this._node.parentNode instanceof SVGElement ? new c.Svg(this._node.parentNode) : null } function h() { for (var a = this._node; "svg" !== a.nodeName;)a = a.parentNode; return new c.Svg(a) } function i(a) { var b = this._node.querySelector(a); return b ? new c.Svg(b) : null } function j(a) { var b = this._node.querySelectorAll(a); return b.length ? new c.Svg.List(b) : null } function k() { return this._node } function l(a, d, e, f) { if ("string" == typeof a) { var g = b.createElement("div"); g.innerHTML = a, a = g.firstChild } a.setAttribute("xmlns", c.namespaces.xmlns); var h = this.elem("foreignObject", d, e, f); return h._node.appendChild(a), h } function m(a) { return this._node.appendChild(b.createTextNode(a)), this } function n() { for (; this._node.firstChild;)this._node.removeChild(this._node.firstChild); return this } function o() { return this._node.parentNode.removeChild(this._node), this.parent() } function p(a) { return this._node.parentNode.replaceChild(a._node, this._node), a } function q(a, b) { return b && this._node.firstChild ? this._node.insertBefore(a._node, this._node.firstChild) : this._node.appendChild(a._node), this } function r() { return this._node.getAttribute("class") ? this._node.getAttribute("class").trim().split(/\s+/) : [] } function s(a) { return this._node.setAttribute("class", this.classes(this._node).concat(a.trim().split(/\s+/)).filter(function (a, b, c) { return c.indexOf(a) === b }).join(" ")), this } function t(a) { var b = a.trim().split(/\s+/); return this._node.setAttribute("class", this.classes(this._node).filter(function (a) { return b.indexOf(a) === -1 }).join(" ")), this } function u() { return this._node.setAttribute("class", ""), this } function v() { return this._node.getBoundingClientRect().height } function w() { return this._node.getBoundingClientRect().width } function x(a, b, d) { return void 0 === b && (b = !0), Object.keys(a).forEach(function (e) { function f(a, b) { var f, g, h, i = {}; a.easing && (h = a.easing instanceof Array ? a.easing : c.Svg.Easing[a.easing], delete a.easing), a.begin = c.ensureUnit(a.begin, "ms"), a.dur = c.ensureUnit(a.dur, "ms"), h && (a.calcMode = "spline", a.keySplines = h.join(" "), a.keyTimes = "0;1"), b && (a.fill = "freeze", i[e] = a.from, this.attr(i), g = c.quantity(a.begin || 0).value, a.begin = "indefinite"), f = this.elem("animate", c.extend({ attributeName: e }, a)), b && setTimeout(function () { try { f._node.beginElement() } catch (b) { i[e] = a.to, this.attr(i), f.remove() } }.bind(this), g), d && f._node.addEventListener("beginEvent", function () { d.emit("animationBegin", { element: this, animate: f._node, params: a }) }.bind(this)), f._node.addEventListener("endEvent", function () { d && d.emit("animationEnd", { element: this, animate: f._node, params: a }), b && (i[e] = a.to, this.attr(i), f.remove()) }.bind(this)) } a[e] instanceof Array ? a[e].forEach(function (a) { f.bind(this)(a, !1) }.bind(this)) : f.bind(this)(a[e], b) }.bind(this)), this } function y(a) { var b = this; this.svgElements = []; for (var d = 0; d < a.length; d++)this.svgElements.push(new c.Svg(a[d])); Object.keys(c.Svg.prototype).filter(function (a) { return ["constructor", "parent", "querySelector", "querySelectorAll", "replace", "append", "classes", "height", "width"].indexOf(a) === -1 }).forEach(function (a) { b[a] = function () { var d = Array.prototype.slice.call(arguments, 0); return b.svgElements.forEach(function (b) { c.Svg.prototype[a].apply(b, d) }), b } }) } c.Svg = c.Class.extend({ constructor: d, attr: e, elem: f, parent: g, root: h, querySelector: i, querySelectorAll: j, getNode: k, foreignObject: l, text: m, empty: n, remove: o, replace: p, append: q, classes: r, addClass: s, removeClass: t, removeAllClasses: u, height: v, width: w, animate: x }), c.Svg.isSupported = function (a) { return b.implementation.hasFeature("http://www.w3.org/TR/SVG11/feature#" + a, "1.1") }; var z = { easeInSine: [.47, 0, .745, .715], easeOutSine: [.39, .575, .565, 1], easeInOutSine: [.445, .05, .55, .95], easeInQuad: [.55, .085, .68, .53], easeOutQuad: [.25, .46, .45, .94], easeInOutQuad: [.455, .03, .515, .955], easeInCubic: [.55, .055, .675, .19], easeOutCubic: [.215, .61, .355, 1], easeInOutCubic: [.645, .045, .355, 1], easeInQuart: [.895, .03, .685, .22], easeOutQuart: [.165, .84, .44, 1], easeInOutQuart: [.77, 0, .175, 1], easeInQuint: [.755, .05, .855, .06], easeOutQuint: [.23, 1, .32, 1], easeInOutQuint: [.86, 0, .07, 1], easeInExpo: [.95, .05, .795, .035], easeOutExpo: [.19, 1, .22, 1], easeInOutExpo: [1, 0, 0, 1], easeInCirc: [.6, .04, .98, .335], easeOutCirc: [.075, .82, .165, 1], easeInOutCirc: [.785, .135, .15, .86], easeInBack: [.6, -.28, .735, .045], easeOutBack: [.175, .885, .32, 1.275], easeInOutBack: [.68, -.55, .265, 1.55] }; c.Svg.Easing = z, c.Svg.List = c.Class.extend({ constructor: y }) }(window, document, a), function (a, b, c) { "use strict"; function d(a, b, d, e, f, g) { var h = c.extend({ command: f ? a.toLowerCase() : a.toUpperCase() }, b, g ? { data: g } : {}); d.splice(e, 0, h) } function e(a, b) { a.forEach(function (c, d) { u[c.command.toLowerCase()].forEach(function (e, f) { b(c, e, d, f, a) }) }) } function f(a, b) { this.pathElements = [], this.pos = 0, this.close = a, this.options = c.extend({}, v, b) } function g(a) { return void 0 !== a ? (this.pos = Math.max(0, Math.min(this.pathElements.length, a)), this) : this.pos } function h(a) { return this.pathElements.splice(this.pos, a), this } function i(a, b, c, e) { return d("M", { x: +a, y: +b }, this.pathElements, this.pos++, c, e), this } function j(a, b, c, e) { return d("L", { x: +a, y: +b }, this.pathElements, this.pos++, c, e), this } function k(a, b, c, e, f, g, h, i) { return d("C", { x1: +a, y1: +b, x2: +c, y2: +e, x: +f, y: +g }, this.pathElements, this.pos++, h, i), this } function l(a, b, c, e, f, g, h, i, j) { return d("A", { rx: +a, ry: +b, xAr: +c, lAf: +e, sf: +f, x: +g, y: +h }, this.pathElements, this.pos++, i, j), this } function m(a) { var b = a.replace(/([A-Za-z])([0-9])/g, "$1 $2").replace(/([0-9])([A-Za-z])/g, "$1 $2").split(/[\s,]+/).reduce(function (a, b) { return b.match(/[A-Za-z]/) && a.push([]), a[a.length - 1].push(b), a }, []); "Z" === b[b.length - 1][0].toUpperCase() && b.pop(); var d = b.map(function (a) { var b = a.shift(), d = u[b.toLowerCase()]; return c.extend({ command: b }, d.reduce(function (b, c, d) { return b[c] = +a[d], b }, {})) }), e = [this.pos, 0]; return Array.prototype.push.apply(e, d), Array.prototype.splice.apply(this.pathElements, e), this.pos += d.length, this } function n() { var a = Math.pow(10, this.options.accuracy); return this.pathElements.reduce(function (b, c) { var d = u[c.command.toLowerCase()].map(function (b) { return this.options.accuracy ? Math.round(c[b] * a) / a : c[b] }.bind(this)); return b + c.command + d.join(",") }.bind(this), "") + (this.close ? "Z" : "") } function o(a, b) { return e(this.pathElements, function (c, d) { c[d] *= "x" === d[0] ? a : b }), this } function p(a, b) { return e(this.pathElements, function (c, d) { c[d] += "x" === d[0] ? a : b }), this } function q(a) { return e(this.pathElements, function (b, c, d, e, f) { var g = a(b, c, d, e, f); (g || 0 === g) && (b[c] = g) }), this } function r(a) { var b = new c.Svg.Path(a || this.close); return b.pos = this.pos, b.pathElements = this.pathElements.slice().map(function (a) { return c.extend({}, a) }), b.options = c.extend({}, this.options), b } function s(a) { var b = [new c.Svg.Path]; return this.pathElements.forEach(function (d) { d.command === a.toUpperCase() && 0 !== b[b.length - 1].pathElements.length && b.push(new c.Svg.Path), b[b.length - 1].pathElements.push(d) }), b } function t(a, b, d) { for (var e = new c.Svg.Path(b, d), f = 0; f < a.length; f++)for (var g = a[f], h = 0; h < g.pathElements.length; h++)e.pathElements.push(g.pathElements[h]); return e } var u = { m: ["x", "y"], l: ["x", "y"], c: ["x1", "y1", "x2", "y2", "x", "y"], a: ["rx", "ry", "xAr", "lAf", "sf", "x", "y"] }, v = { accuracy: 3 }; c.Svg.Path = c.Class.extend({ constructor: f, position: g, remove: h, move: i, line: j, curve: k, arc: l, scale: o, translate: p, transform: q, parse: m, stringify: n, clone: r, splitByCommand: s }), c.Svg.Path.elementDescriptions = u, c.Svg.Path.join = t }(window, document, a), function (a, b, c) { "use strict"; function d(a, b, c, d) { this.units = a, this.counterUnits = a === f.x ? f.y : f.x, this.chartRect = b, this.axisLength = b[a.rectEnd] - b[a.rectStart], this.gridOffset = b[a.rectOffset], this.ticks = c, this.options = d } function e(a, b, d, e, f) { var g = e["axis" + this.units.pos.toUpperCase()], h = this.ticks.map(this.projectValue.bind(this)), i = this.ticks.map(g.labelInterpolationFnc); h.forEach(function (j, k) { var l, m = { x: 0, y: 0 }; l = h[k + 1] ? h[k + 1] - j : Math.max(this.axisLength - j, 30), c.isFalseyButZero(i[k]) && "" !== i[k] || ("x" === this.units.pos ? (j = this.chartRect.x1 + j, m.x = e.axisX.labelOffset.x, "start" === e.axisX.position ? m.y = this.chartRect.padding.top + e.axisX.labelOffset.y + (d ? 5 : 20) : m.y = this.chartRect.y1 + e.axisX.labelOffset.y + (d ? 5 : 20)) : (j = this.chartRect.y1 - j, m.y = e.axisY.labelOffset.y - (d ? l : 0), "start" === e.axisY.position ? m.x = d ? this.chartRect.padding.left + e.axisY.labelOffset.x : this.chartRect.x1 - 10 : m.x = this.chartRect.x2 + e.axisY.labelOffset.x + 10), g.showGrid && c.createGrid(j, k, this, this.gridOffset, this.chartRect[this.counterUnits.len](), a, [e.classNames.grid, e.classNames[this.units.dir]], f), g.showLabel && c.createLabel(j, l, k, i, this, g.offset, m, b, [e.classNames.label, e.classNames[this.units.dir], "start" === g.position ? e.classNames[g.position] : e.classNames.end], d, f)) }.bind(this)) } var f = { x: { pos: "x", len: "width", dir: "horizontal", rectStart: "x1", rectEnd: "x2", rectOffset: "y2" }, y: { pos: "y", len: "height", dir: "vertical", rectStart: "y2", rectEnd: "y1", rectOffset: "x1" } }; c.Axis = c.Class.extend({ constructor: d, createGridAndLabels: e, projectValue: function (a, b, c) { throw new Error("Base axis can't be instantiated!") } }), c.Axis.units = f }(window, document, a), function (a, b, c) { "use strict"; function d(a, b, d, e) { var f = e.highLow || c.getHighLow(b, e, a.pos); this.bounds = c.getBounds(d[a.rectEnd] - d[a.rectStart], f, e.scaleMinSpace || 20, e.onlyInteger), this.range = { min: this.bounds.min, max: this.bounds.max }, c.AutoScaleAxis["super"].constructor.call(this, a, d, this.bounds.values, e) } function e(a) { return this.axisLength * (+c.getMultiValue(a, this.units.pos) - this.bounds.min) / this.bounds.range } c.AutoScaleAxis = c.Axis.extend({ constructor: d, projectValue: e }) }(window, document, a), function (a, b, c) { "use strict"; function d(a, b, d, e) { var f = e.highLow || c.getHighLow(b, e, a.pos); this.divisor = e.divisor || 1, this.ticks = e.ticks || c.times(this.divisor).map(function (a, b) { return f.low + (f.high - f.low) / this.divisor * b }.bind(this)), this.ticks.sort(function (a, b) { return a - b }), this.range = { min: f.low, max: f.high }, c.FixedScaleAxis["super"].constructor.call(this, a, d, this.ticks, e), this.stepLength = this.axisLength / this.divisor } function e(a) { return this.axisLength * (+c.getMultiValue(a, this.units.pos) - this.range.min) / (this.range.max - this.range.min) } c.FixedScaleAxis = c.Axis.extend({ constructor: d, projectValue: e }) }(window, document, a), function (a, b, c) { "use strict"; function d(a, b, d, e) { c.StepAxis["super"].constructor.call(this, a, d, e.ticks, e); var f = Math.max(1, e.ticks.length - (e.stretch ? 1 : 0)); this.stepLength = this.axisLength / f } function e(a, b) { return this.stepLength * b } c.StepAxis = c.Axis.extend({ constructor: d, projectValue: e }) }(window, document, a), function (a, b, c) { "use strict"; function d(a) { var b = c.normalizeData(this.data, a.reverseData, !0); this.svg = c.createSvg(this.container, a.width, a.height, a.classNames.chart); var d, e, g = this.svg.elem("g").addClass(a.classNames.gridGroup), h = this.svg.elem("g"), i = this.svg.elem("g").addClass(a.classNames.labelGroup), j = c.createChartRect(this.svg, a, f.padding); d = void 0 === a.axisX.type ? new c.StepAxis(c.Axis.units.x, b.normalized.series, j, c.extend({}, a.axisX, { ticks: b.normalized.labels, stretch: a.fullWidth })) : a.axisX.type.call(c, c.Axis.units.x, b.normalized.series, j, a.axisX), e = void 0 === a.axisY.type ? new c.AutoScaleAxis(c.Axis.units.y, b.normalized.series, j, c.extend({}, a.axisY, { high: c.isNumeric(a.high) ? a.high : a.axisY.high, low: c.isNumeric(a.low) ? a.low : a.axisY.low })) : a.axisY.type.call(c, c.Axis.units.y, b.normalized.series, j, a.axisY), d.createGridAndLabels(g, i, this.supportsForeignObject, a, this.eventEmitter), e.createGridAndLabels(g, i, this.supportsForeignObject, a, this.eventEmitter), a.showGridBackground && c.createGridBackground(g, j, a.classNames.gridBackground, this.eventEmitter), b.raw.series.forEach(function (f, g) { var i = h.elem("g"); i.attr({ "ct:series-name": f.name, "ct:meta": c.serialize(f.meta) }), i.addClass([a.classNames.series, f.className || a.classNames.series + "-" + c.alphaNumerate(g)].join(" ")); var k = [], l = []; b.normalized.series[g].forEach(function (a, h) { var i = { x: j.x1 + d.projectValue(a, h, b.normalized.series[g]), y: j.y1 - e.projectValue(a, h, b.normalized.series[g]) }; k.push(i.x, i.y), l.push({ value: a, valueIndex: h, meta: c.getMetaData(f, h) }) }.bind(this)); var m = { lineSmooth: c.getSeriesOption(f, a, "lineSmooth"), showPoint: c.getSeriesOption(f, a, "showPoint"), showLine: c.getSeriesOption(f, a, "showLine"), showArea: c.getSeriesOption(f, a, "showArea"), areaBase: c.getSeriesOption(f, a, "areaBase") }, n = "function" == typeof m.lineSmooth ? m.lineSmooth : m.lineSmooth ? c.Interpolation.monotoneCubic() : c.Interpolation.none(), o = n(k, l); if (m.showPoint && o.pathElements.forEach(function (b) { var h = i.elem("line", { x1: b.x, y1: b.y, x2: b.x + .01, y2: b.y }, a.classNames.point).attr({ "ct:value": [b.data.value.x, b.data.value.y].filter(c.isNumeric).join(","), "ct:meta": c.serialize(b.data.meta) }); this.eventEmitter.emit("draw", { type: "point", value: b.data.value, index: b.data.valueIndex, meta: b.data.meta, series: f, seriesIndex: g, axisX: d, axisY: e, group: i, element: h, x: b.x, y: b.y }) }.bind(this)), m.showLine) { var p = i.elem("path", { d: o.stringify() }, a.classNames.line, !0); this.eventEmitter.emit("draw", { type: "line", values: b.normalized.series[g], path: o.clone(), chartRect: j, index: g, series: f, seriesIndex: g, seriesMeta: f.meta, axisX: d, axisY: e, group: i, element: p }) } if (m.showArea && e.range) { var q = Math.max(Math.min(m.areaBase, e.range.max), e.range.min), r = j.y1 - e.projectValue(q); o.splitByCommand("M").filter(function (a) { return a.pathElements.length > 1 }).map(function (a) { var b = a.pathElements[0], c = a.pathElements[a.pathElements.length - 1]; return a.clone(!0).position(0).remove(1).move(b.x, r).line(b.x, b.y).position(a.pathElements.length + 1).line(c.x, r) }).forEach(function (c) { var h = i.elem("path", { d: c.stringify() }, a.classNames.area, !0); this.eventEmitter.emit("draw", { type: "area", values: b.normalized.series[g], path: c.clone(), series: f, seriesIndex: g, axisX: d, axisY: e, chartRect: j, index: g, group: i, element: h }) }.bind(this)) } }.bind(this)), this.eventEmitter.emit("created", { bounds: e.bounds, chartRect: j, axisX: d, axisY: e, svg: this.svg, options: a }) } function e(a, b, d, e) { c.Line["super"].constructor.call(this, a, b, f, c.extend({}, f, d), e) } var f = { axisX: { offset: 30, position: "end", labelOffset: { x: 0, y: 0 }, showLabel: !0, showGrid: !0, labelInterpolationFnc: c.noop, type: void 0 }, axisY: { offset: 40, position: "start", labelOffset: { x: 0, y: 0 }, showLabel: !0, showGrid: !0, labelInterpolationFnc: c.noop, type: void 0, scaleMinSpace: 20, onlyInteger: !1 }, width: void 0, height: void 0, showLine: !0, showPoint: !0, showArea: !1, areaBase: 0, lineSmooth: !0, showGridBackground: !1, low: void 0, high: void 0, chartPadding: { top: 15, right: 15, bottom: 5, left: 10 }, fullWidth: !1, reverseData: !1, classNames: { chart: "ct-chart-line", label: "ct-label", labelGroup: "ct-labels", series: "ct-series", line: "ct-line", point: "ct-point", area: "ct-area", grid: "ct-grid", gridGroup: "ct-grids", gridBackground: "ct-grid-background", vertical: "ct-vertical", horizontal: "ct-horizontal", start: "ct-start", end: "ct-end" } }; c.Line = c.Base.extend({ constructor: e, createChart: d }) }(window, document, a), function (a, b, c) {
        "use strict"; function d(a) {
            var b, d; a.distributeSeries ? (b = c.normalizeData(this.data, a.reverseData, a.horizontalBars ? "x" : "y"), b.normalized.series = b.normalized.series.map(function (a) { return [a] })) : b = c.normalizeData(this.data, a.reverseData, a.horizontalBars ? "x" : "y"), this.svg = c.createSvg(this.container, a.width, a.height, a.classNames.chart + (a.horizontalBars ? " " + a.classNames.horizontalBars : "")); var e = this.svg.elem("g").addClass(a.classNames.gridGroup), g = this.svg.elem("g"), h = this.svg.elem("g").addClass(a.classNames.labelGroup); if (a.stackBars && 0 !== b.normalized.series.length) {
                var i = c.serialMap(b.normalized.series, function () {
                    return Array.prototype.slice.call(arguments).map(function (a) { return a }).reduce(function (a, b) { return { x: a.x + (b && b.x) || 0, y: a.y + (b && b.y) || 0 } }, { x: 0, y: 0 })
                }); d = c.getHighLow([i], a, a.horizontalBars ? "x" : "y")
            } else d = c.getHighLow(b.normalized.series, a, a.horizontalBars ? "x" : "y"); d.high = +a.high || (0 === a.high ? 0 : d.high), d.low = +a.low || (0 === a.low ? 0 : d.low); var j, k, l, m, n, o = c.createChartRect(this.svg, a, f.padding); k = a.distributeSeries && a.stackBars ? b.normalized.labels.slice(0, 1) : b.normalized.labels, a.horizontalBars ? (j = m = void 0 === a.axisX.type ? new c.AutoScaleAxis(c.Axis.units.x, b.normalized.series, o, c.extend({}, a.axisX, { highLow: d, referenceValue: 0 })) : a.axisX.type.call(c, c.Axis.units.x, b.normalized.series, o, c.extend({}, a.axisX, { highLow: d, referenceValue: 0 })), l = n = void 0 === a.axisY.type ? new c.StepAxis(c.Axis.units.y, b.normalized.series, o, { ticks: k }) : a.axisY.type.call(c, c.Axis.units.y, b.normalized.series, o, a.axisY)) : (l = m = void 0 === a.axisX.type ? new c.StepAxis(c.Axis.units.x, b.normalized.series, o, { ticks: k }) : a.axisX.type.call(c, c.Axis.units.x, b.normalized.series, o, a.axisX), j = n = void 0 === a.axisY.type ? new c.AutoScaleAxis(c.Axis.units.y, b.normalized.series, o, c.extend({}, a.axisY, { highLow: d, referenceValue: 0 })) : a.axisY.type.call(c, c.Axis.units.y, b.normalized.series, o, c.extend({}, a.axisY, { highLow: d, referenceValue: 0 }))); var p = a.horizontalBars ? o.x1 + j.projectValue(0) : o.y1 - j.projectValue(0), q = []; l.createGridAndLabels(e, h, this.supportsForeignObject, a, this.eventEmitter), j.createGridAndLabels(e, h, this.supportsForeignObject, a, this.eventEmitter), a.showGridBackground && c.createGridBackground(e, o, a.classNames.gridBackground, this.eventEmitter), b.raw.series.forEach(function (d, e) { var f, h, i = e - (b.raw.series.length - 1) / 2; f = a.distributeSeries && !a.stackBars ? l.axisLength / b.normalized.series.length / 2 : a.distributeSeries && a.stackBars ? l.axisLength / 2 : l.axisLength / b.normalized.series[e].length / 2, h = g.elem("g"), h.attr({ "ct:series-name": d.name, "ct:meta": c.serialize(d.meta) }), h.addClass([a.classNames.series, d.className || a.classNames.series + "-" + c.alphaNumerate(e)].join(" ")), b.normalized.series[e].forEach(function (g, k) { var r, s, t, u; if (u = a.distributeSeries && !a.stackBars ? e : a.distributeSeries && a.stackBars ? 0 : k, r = a.horizontalBars ? { x: o.x1 + j.projectValue(g && g.x ? g.x : 0, k, b.normalized.series[e]), y: o.y1 - l.projectValue(g && g.y ? g.y : 0, u, b.normalized.series[e]) } : { x: o.x1 + l.projectValue(g && g.x ? g.x : 0, u, b.normalized.series[e]), y: o.y1 - j.projectValue(g && g.y ? g.y : 0, k, b.normalized.series[e]) }, l instanceof c.StepAxis && (l.options.stretch || (r[l.units.pos] += f * (a.horizontalBars ? -1 : 1)), r[l.units.pos] += a.stackBars || a.distributeSeries ? 0 : i * a.seriesBarDistance * (a.horizontalBars ? -1 : 1)), t = q[k] || p, q[k] = t - (p - r[l.counterUnits.pos]), void 0 !== g) { var v = {}; v[l.units.pos + "1"] = r[l.units.pos], v[l.units.pos + "2"] = r[l.units.pos], !a.stackBars || "accumulate" !== a.stackMode && a.stackMode ? (v[l.counterUnits.pos + "1"] = p, v[l.counterUnits.pos + "2"] = r[l.counterUnits.pos]) : (v[l.counterUnits.pos + "1"] = t, v[l.counterUnits.pos + "2"] = q[k]), v.x1 = Math.min(Math.max(v.x1, o.x1), o.x2), v.x2 = Math.min(Math.max(v.x2, o.x1), o.x2), v.y1 = Math.min(Math.max(v.y1, o.y2), o.y1), v.y2 = Math.min(Math.max(v.y2, o.y2), o.y1); var w = c.getMetaData(d, k); s = h.elem("line", v, a.classNames.bar).attr({ "ct:value": [g.x, g.y].filter(c.isNumeric).join(","), "ct:meta": c.serialize(w) }), this.eventEmitter.emit("draw", c.extend({ type: "bar", value: g, index: k, meta: w, series: d, seriesIndex: e, axisX: m, axisY: n, chartRect: o, group: h, element: s }, v)) } }.bind(this)) }.bind(this)), this.eventEmitter.emit("created", { bounds: j.bounds, chartRect: o, axisX: m, axisY: n, svg: this.svg, options: a })
        } function e(a, b, d, e) { c.Bar["super"].constructor.call(this, a, b, f, c.extend({}, f, d), e) } var f = { axisX: { offset: 30, position: "end", labelOffset: { x: 0, y: 0 }, showLabel: !0, showGrid: !0, labelInterpolationFnc: c.noop, scaleMinSpace: 30, onlyInteger: !1 }, axisY: { offset: 40, position: "start", labelOffset: { x: 0, y: 0 }, showLabel: !0, showGrid: !0, labelInterpolationFnc: c.noop, scaleMinSpace: 20, onlyInteger: !1 }, width: void 0, height: void 0, high: void 0, low: void 0, referenceValue: 0, chartPadding: { top: 15, right: 15, bottom: 5, left: 10 }, seriesBarDistance: 15, stackBars: !1, stackMode: "accumulate", horizontalBars: !1, distributeSeries: !1, reverseData: !1, showGridBackground: !1, classNames: { chart: "ct-chart-bar", horizontalBars: "ct-horizontal-bars", label: "ct-label", labelGroup: "ct-labels", series: "ct-series", bar: "ct-bar", grid: "ct-grid", gridGroup: "ct-grids", gridBackground: "ct-grid-background", vertical: "ct-vertical", horizontal: "ct-horizontal", start: "ct-start", end: "ct-end" } }; c.Bar = c.Base.extend({ constructor: e, createChart: d })
    }(window, document, a), function (a, b, c) { "use strict"; function d(a, b, c) { var d = b.x > a.x; return d && "explode" === c || !d && "implode" === c ? "start" : d && "implode" === c || !d && "explode" === c ? "end" : "middle" } function e(a) { var b, e, f, h, i, j = c.normalizeData(this.data), k = [], l = a.startAngle; this.svg = c.createSvg(this.container, a.width, a.height, a.donut ? a.classNames.chartDonut : a.classNames.chartPie), e = c.createChartRect(this.svg, a, g.padding), f = Math.min(e.width() / 2, e.height() / 2), i = a.total || j.normalized.series.reduce(function (a, b) { return a + b }, 0); var m = c.quantity(a.donutWidth); "%" === m.unit && (m.value *= f / 100), f -= a.donut && !a.donutSolid ? m.value / 2 : 0, h = "outside" === a.labelPosition || a.donut && !a.donutSolid ? f : "center" === a.labelPosition ? 0 : a.donutSolid ? f - m.value / 2 : f / 2, h += a.labelOffset; var n = { x: e.x1 + e.width() / 2, y: e.y2 + e.height() / 2 }, o = 1 === j.raw.series.filter(function (a) { return a.hasOwnProperty("value") ? 0 !== a.value : 0 !== a }).length; j.raw.series.forEach(function (a, b) { k[b] = this.svg.elem("g", null, null) }.bind(this)), a.showLabel && (b = this.svg.elem("g", null, null)), j.raw.series.forEach(function (e, g) { if (0 !== j.normalized.series[g] || !a.ignoreEmptyValues) { k[g].attr({ "ct:series-name": e.name }), k[g].addClass([a.classNames.series, e.className || a.classNames.series + "-" + c.alphaNumerate(g)].join(" ")); var p = i > 0 ? l + j.normalized.series[g] / i * 360 : 0, q = Math.max(0, l - (0 === g || o ? 0 : .2)); p - q >= 359.99 && (p = q + 359.99); var r, s, t, u = c.polarToCartesian(n.x, n.y, f, q), v = c.polarToCartesian(n.x, n.y, f, p), w = new c.Svg.Path(!a.donut || a.donutSolid).move(v.x, v.y).arc(f, f, 0, p - l > 180, 0, u.x, u.y); a.donut ? a.donutSolid && (t = f - m.value, r = c.polarToCartesian(n.x, n.y, t, l - (0 === g || o ? 0 : .2)), s = c.polarToCartesian(n.x, n.y, t, p), w.line(r.x, r.y), w.arc(t, t, 0, p - l > 180, 1, s.x, s.y)) : w.line(n.x, n.y); var x = a.classNames.slicePie; a.donut && (x = a.classNames.sliceDonut, a.donutSolid && (x = a.classNames.sliceDonutSolid)); var y = k[g].elem("path", { d: w.stringify() }, x); if (y.attr({ "ct:value": j.normalized.series[g], "ct:meta": c.serialize(e.meta) }), a.donut && !a.donutSolid && (y._node.style.strokeWidth = m.value + "px"), this.eventEmitter.emit("draw", { type: "slice", value: j.normalized.series[g], totalDataSum: i, index: g, meta: e.meta, series: e, group: k[g], element: y, path: w.clone(), center: n, radius: f, startAngle: l, endAngle: p }), a.showLabel) { var z; z = 1 === j.raw.series.length ? { x: n.x, y: n.y } : c.polarToCartesian(n.x, n.y, h, l + (p - l) / 2); var A; A = j.normalized.labels && !c.isFalseyButZero(j.normalized.labels[g]) ? j.normalized.labels[g] : j.normalized.series[g]; var B = a.labelInterpolationFnc(A, g); if (B || 0 === B) { var C = b.elem("text", { dx: z.x, dy: z.y, "text-anchor": d(n, z, a.labelDirection) }, a.classNames.label).text("" + B); this.eventEmitter.emit("draw", { type: "label", index: g, group: b, element: C, text: "" + B, x: z.x, y: z.y }) } } l = p } }.bind(this)), this.eventEmitter.emit("created", { chartRect: e, svg: this.svg, options: a }) } function f(a, b, d, e) { c.Pie["super"].constructor.call(this, a, b, g, c.extend({}, g, d), e) } var g = { width: void 0, height: void 0, chartPadding: 5, classNames: { chartPie: "ct-chart-pie", chartDonut: "ct-chart-donut", series: "ct-series", slicePie: "ct-slice-pie", sliceDonut: "ct-slice-donut", sliceDonutSolid: "ct-slice-donut-solid", label: "ct-label" }, startAngle: 0, total: void 0, donut: !1, donutSolid: !1, donutWidth: 60, showLabel: !0, labelOffset: 0, labelPosition: "inside", labelInterpolationFnc: c.noop, labelDirection: "neutral", reverseData: !1, ignoreEmptyValues: !1 }; c.Pie = c.Base.extend({ constructor: f, createChart: e, determineAnchorPosition: d }) }(window, document, a), a
});

var i, l, selectedLine = null;

/* Navigate to hash without browser history entry */
var navigateToHash = function () {
    if (window.history !== undefined && window.history.replaceState !== undefined) {
        window.history.replaceState(undefined, undefined, this.getAttribute("href"));
    }
};

var hashLinks = document.getElementsByClassName('navigatetohash');
for (i = 0, l = hashLinks.length; i < l; i++) {
    hashLinks[i].addEventListener('click', navigateToHash);
}

/* Switch test method */
var switchTestMethod = function () {
    var method = this.getAttribute("value");
    console.log("Selected test method: " + method);

    var lines, i, l, coverageData, lineAnalysis, cells;

    lines = document.querySelectorAll('.lineAnalysis tr');

    for (i = 1, l = lines.length; i < l; i++) {
        coverageData = JSON.parse(lines[i].getAttribute('data-coverage').replace(/'/g, '"'));
        lineAnalysis = coverageData[method];
        cells = lines[i].querySelectorAll('td');
        if (lineAnalysis === undefined) {
            lineAnalysis = coverageData.AllTestMethods;
            if (lineAnalysis.LVS !== 'gray') {
                cells[0].setAttribute('class', 'red');
                cells[1].innerText = cells[1].textContent = '0';
                cells[4].setAttribute('class', 'lightred');
            }
        } else {
            cells[0].setAttribute('class', lineAnalysis.LVS);
            cells[1].innerText = cells[1].textContent = lineAnalysis.VC;
            cells[4].setAttribute('class', 'light' + lineAnalysis.LVS);
        }
    }
};

var testMethods = document.getElementsByClassName('switchtestmethod');
for (i = 0, l = testMethods.length; i < l; i++) {
    testMethods[i].addEventListener('change', switchTestMethod);
}

/* Highlight test method by line */
var toggleLine = function () {
    if (selectedLine === this) {
        selectedLine = null;
    } else {
        selectedLine = null;
        unhighlightTestMethods();
        highlightTestMethods.call(this);
        selectedLine = this;
    }
    
};
var highlightTestMethods = function () {
    if (selectedLine !== null) {
        return;
    }

    var lineAnalysis;
    var coverageData = JSON.parse(this.getAttribute('data-coverage').replace(/'/g, '"'));
    var testMethods = document.getElementsByClassName('testmethod');

    for (i = 0, l = testMethods.length; i < l; i++) {
        lineAnalysis = coverageData[testMethods[i].id];
        if (lineAnalysis === undefined) {
            testMethods[i].className = testMethods[i].className.replace(/\s*light.+/g, "");
        } else {
            testMethods[i].className += ' light' + lineAnalysis.LVS;
        }
    }
};
var unhighlightTestMethods = function () {
    if (selectedLine !== null) {
        return;
    }

    var testMethods = document.getElementsByClassName('testmethod');
    for (i = 0, l = testMethods.length; i < l; i++) {
        testMethods[i].className = testMethods[i].className.replace(/\s*light.+/g, "");
    }
};
var coverableLines = document.getElementsByClassName('coverableline');
for (i = 0, l = coverableLines.length; i < l; i++) {
    coverableLines[i].addEventListener('click', toggleLine);
    coverableLines[i].addEventListener('mouseenter', highlightTestMethods);
    coverableLines[i].addEventListener('mouseleave', unhighlightTestMethods);
}

/* History charts */
var renderChart = function (chart) {
    // Remove current children (e.g. PNG placeholder)
    while (chart.firstChild) {
        chart.firstChild.remove();
    }

    var chartData = window[chart.getAttribute('data-data')];
    var options = {
        axisY: {
            type: undefined,
            onlyInteger: true
        },
        lineSmooth: false,
        low: 0,
        high: 100,
        scaleMinSpace: 20,
        onlyInteger: true,
        fullWidth: true
    };
    var lineChart = new Chartist.Line(chart, {
        labels: [],
        series: chartData.series
    }, options);

    /* Zoom */
    var zoomButtonDiv = document.createElement("div");
    zoomButtonDiv.className = "toggleZoom";
    var zoomButtonLink = document.createElement("a");
    zoomButtonLink.setAttribute("href", "");
    var zoomButtonText = document.createElement("i");
    zoomButtonText.className = "icon-search-plus";

    zoomButtonLink.appendChild(zoomButtonText);
    zoomButtonDiv.appendChild(zoomButtonLink);

    chart.appendChild(zoomButtonDiv);

    zoomButtonDiv.addEventListener('click', function (event) {
        event.preventDefault();

        if (options.axisY.type === undefined) {
            options.axisY.type = Chartist.AutoScaleAxis;
            zoomButtonText.className = "icon-search-minus";
        } else {
            options.axisY.type = undefined;
            zoomButtonText.className = "icon-search-plus";
        }

        lineChart.update(null, options);
    });

    var tooltip = document.createElement("div");
    tooltip.className = "tooltip";

    chart.appendChild(tooltip);

    /* Tooltips */
    var showToolTip = function () {
        var point = this;
        var index = [].slice.call(chart.getElementsByClassName('ct-point')).indexOf(point);

        tooltip.innerHTML = chartData.tooltips[index % chartData.tooltips.length];
        tooltip.style.display = 'block';
    };

    var moveToolTip = function (event) {
        var box = chart.getBoundingClientRect();
        var left = event.pageX - box.left - window.pageXOffset;
        var top = event.pageY - box.top - window.pageYOffset;

        left = left + 20;
        top = top - tooltip.offsetHeight / 2;

        if (left + tooltip.offsetWidth > box.width) {
            left -= tooltip.offsetWidth + 40;
        }

        if (top < 0) {
            top = 0;
        }

        if (top + tooltip.offsetHeight > box.height) {
            top = box.height - tooltip.offsetHeight;
        }

        tooltip.style.left = left + 'px';
        tooltip.style.top = top + 'px';
    };

    var hideToolTip = function () {
        tooltip.style.display = 'none';
    };
    chart.addEventListener('mousemove', moveToolTip);

    lineChart.on('created', function () {
        var chartPoints = chart.getElementsByClassName('ct-point');
        for (i = 0, l = chartPoints.length; i < l; i++) {
            chartPoints[i].addEventListener('mousemove', showToolTip);
            chartPoints[i].addEventListener('mouseout', hideToolTip);
        }
    });
};

var charts = document.getElementsByClassName('historychart');
for (i = 0, l = charts.length; i < l; i++) {
    renderChart(charts[i]);
}

var assemblies = [
  {
    "name": "AltCover.Monitor",
    "classes": [
      { "name": "AltCover.Carrier", "rp": "AltCover.Monitor_Carrier.html", "cl": 0, "ucl": 13, "cal": 13, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 8, "lch": [], "bch": [], "hc": [] },
      { "name": "AltCover.Monitor", "rp": "AltCover.Monitor_Monitor.html", "cl": 0, "ucl": 47, "cal": 47, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 18, "lch": [], "bch": [], "hc": [] },
    ]},
  {
    "name": "FSharp.Core",
    "classes": [
      { "name": "Microsoft.FSharp.Collections.Array2DModule", "rp": "FSharp.Core_Array2DModule.html", "cl": 0, "ucl": 63, "cal": 63, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 64, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.Array3DModule", "rp": "FSharp.Core_Array3DModule.html", "cl": 0, "ucl": 56, "cal": 56, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 78, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.Array4DModule", "rp": "FSharp.Core_Array4DModule.html", "cl": 0, "ucl": 26, "cal": 26, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 40, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.ArrayModule", "rp": "FSharp.Core_ArrayModule.html", "cl": 8, "ucl": 753, "cal": 761, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 1, "tb": 647, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.CachedSeq<T>", "rp": "FSharp.Core_CachedSeq_1.html", "cl": 0, "ucl": 5, "cal": 5, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.ComparisonIdentity", "rp": "FSharp.Core_ComparisonIdentity.html", "cl": 0, "ucl": 6, "cal": 6, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 8, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.FSharpList<T>", "rp": "FSharp.Core_FSharpList_1.html", "cl": 2, "ucl": 21, "cal": 23, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 6, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.FSharpMap<T1, T2>", "rp": "FSharp.Core_FSharpMap_2.html", "cl": 10, "ucl": 93, "cal": 103, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 18, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.FSharpSet<T>", "rp": "FSharp.Core_FSharpSet_1.html", "cl": 13, "ucl": 83, "cal": 96, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 3, "tb": 28, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.Generator", "rp": "FSharp.Core_Generator.html", "cl": 0, "ucl": 60, "cal": 60, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 18, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.HashIdentity", "rp": "FSharp.Core_HashIdentity.html", "cl": 0, "ucl": 12, "cal": 12, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.IEnumerator", "rp": "FSharp.Core_IEnumerator.html", "cl": 9, "ucl": 72, "cal": 81, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 1, "tb": 18, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.Internal", "rp": "FSharp.Core_Internal.html", "cl": 53, "ucl": 142, "cal": 195, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 17, "tb": 69, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.KeyCollection<T1, T2>", "rp": "FSharp.Core_KeyCollection_2.html", "cl": 0, "ucl": 16, "cal": 16, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 12, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.KeyValuePairDebugFriendly<T1, T2>", "rp": "FSharp.Core_KeyValuePairDebugFriendly_2.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.ListDebugView<T>", "rp": "FSharp.Core_ListDebugView_1.html", "cl": 0, "ucl": 10, "cal": 10, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 2, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.ListModule", "rp": "FSharp.Core_ListModule.html", "cl": 11, "ucl": 335, "cal": 346, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 170, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.MapDebugView<T1, T2>", "rp": "FSharp.Core_MapDebugView_2.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 2, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.MapModule", "rp": "FSharp.Core_MapModule.html", "cl": 2, "ucl": 29, "cal": 31, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 4, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.MapTree<T1, T2>", "rp": "FSharp.Core_MapTree_2.html", "cl": 0, "ucl": 5, "cal": 5, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.MapTreeModule", "rp": "FSharp.Core_MapTreeModule.html", "cl": 5, "ucl": 242, "cal": 247, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 3, "tb": 168, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.MapTreeNode<T1, T2>", "rp": "FSharp.Core_MapTreeNode_2.html", "cl": 0, "ucl": 4, "cal": 4, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.PrivateListHelpers", "rp": "FSharp.Core_PrivateListHelpers.html", "cl": 15, "ucl": 31, "cal": 46, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 5, "tb": 26, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.SeqModule", "rp": "FSharp.Core_SeqModule.html", "cl": 17, "ucl": 506, "cal": 523, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 7, "tb": 312, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.SetDebugView<T>", "rp": "FSharp.Core_SetDebugView_1.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.SetModule", "rp": "FSharp.Core_SetModule.html", "cl": 2, "ucl": 29, "cal": 31, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.SetTree<T>", "rp": "FSharp.Core_SetTree_1.html", "cl": 2, "ucl": 2, "cal": 4, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.SetTreeModule", "rp": "FSharp.Core_SetTreeModule.html", "cl": 38, "ucl": 222, "cal": 260, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 27, "tb": 208, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.SetTreeNode<T>", "rp": "FSharp.Core_SetTreeNode_1.html", "cl": 2, "ucl": 2, "cal": 4, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Collections.ValueCollection<T1, T2>", "rp": "FSharp.Core_ValueCollection_2.html", "cl": 0, "ucl": 16, "cal": 16, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 12, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.AsyncActivation<T>", "rp": "FSharp.Core_AsyncActivation_1.html", "cl": 6, "ucl": 28, "cal": 34, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 2, "tb": 6, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.AsyncBuilderImpl", "rp": "FSharp.Core_AsyncBuilderImpl.html", "cl": 0, "ucl": 0, "cal": 0, "tl": 0, "ct": "MethodCoverage", "mc": 0, "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.AsyncHelpers", "rp": "FSharp.Core_AsyncHelpers.html", "cl": 0, "ucl": 21, "cal": 21, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.AsyncPrimitives", "rp": "FSharp.Core_AsyncPrimitives.html", "cl": 142, "ucl": 213, "cal": 355, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 27, "tb": 106, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.AsyncResult<T>", "rp": "FSharp.Core_AsyncResult_1.html", "cl": 1, "ucl": 2, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.AsyncReturn", "rp": "FSharp.Core_AsyncReturn.html", "cl": 0, "ucl": 1, "cal": 1, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.BackgroundTaskBuilder", "rp": "FSharp.Core_BackgroundTaskBuilder.html", "cl": 0, "ucl": 6, "cal": 6, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 4, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.CommonExtensions", "rp": "FSharp.Core_CommonExtensions.html", "cl": 5, "ucl": 15, "cal": 20, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 2, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.EventDelegee<T>", "rp": "FSharp.Core_EventDelegee_1.html", "cl": 0, "ucl": 11, "cal": 11, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 2, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.EventModule", "rp": "FSharp.Core_EventModule.html", "cl": 0, "ucl": 36, "cal": 36, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 6, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.ExceptionDispatchInfoHelpers", "rp": "FSharp.Core_ExceptionDispatchInfoHelpers.html", "cl": 0, "ucl": 8, "cal": 8, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 2, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.FSharpAsync", "rp": "FSharp.Core_FSharpAsync.html", "cl": 2, "ucl": 490, "cal": 492, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 1, "tb": 98, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.FSharpAsyncBuilder", "rp": "FSharp.Core_FSharpAsyncBuilder.html", "cl": 6, "ucl": 6, "cal": 12, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.FSharpAsyncReplyChannel<T>", "rp": "FSharp.Core_FSharpAsyncReplyChannel_1.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.FSharpDelegateEvent<T>", "rp": "FSharp.Core_FSharpDelegateEvent_1.html", "cl": 0, "ucl": 10, "cal": 10, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.FSharpEvent<T>", "rp": "FSharp.Core_FSharpEvent_1.html", "cl": 17, "ucl": 0, "cal": 17, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.FSharpEvent<T1, T2>", "rp": "FSharp.Core_FSharpEvent_2.html", "cl": 0, "ucl": 40, "cal": 40, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 16, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.FSharpMailboxProcessor<T>", "rp": "FSharp.Core_FSharpMailboxProcessor_1.html", "cl": 0, "ucl": 61, "cal": 61, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 12, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.Latch", "rp": "FSharp.Core_Latch.html", "cl": 0, "ucl": 3, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.LazyExtensions", "rp": "FSharp.Core_LazyExtensions.html", "cl": 2, "ucl": 6, "cal": 8, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.LinkedSubSource", "rp": "FSharp.Core_LinkedSubSource.html", "cl": 0, "ucl": 8, "cal": 8, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.Mailbox<T>", "rp": "FSharp.Core_Mailbox_1.html", "cl": 0, "ucl": 157, "cal": 157, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 42, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.ObservableModule", "rp": "FSharp.Core_ObservableModule.html", "cl": 0, "ucl": 115, "cal": 115, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 38, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.TaskBuilder", "rp": "FSharp.Core_TaskBuilder.html", "cl": 0, "ucl": 23, "cal": 23, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 4, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.TaskBuilderBase", "rp": "FSharp.Core_TaskBuilderBase.html", "cl": 0, "ucl": 48, "cal": 48, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 6, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.TaskBuilderExtensions.HighPriority", "rp": "FSharp.Core_HighPriority.html", "cl": 0, "ucl": 33, "cal": 33, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 2, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.TaskBuilderExtensions.LowPriority", "rp": "FSharp.Core_LowPriority.html", "cl": 0, "ucl": 35, "cal": 35, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 12, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.TaskBuilderExtensions.MediumPriority", "rp": "FSharp.Core_MediumPriority.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.TaskBuilderModule", "rp": "FSharp.Core_TaskBuilderModule.html", "cl": 0, "ucl": 0, "cal": 0, "tl": 0, "ct": "MethodCoverage", "mc": 0, "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.Trampoline", "rp": "FSharp.Core_Trampoline.html", "cl": 19, "ucl": 13, "cal": 32, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 3, "tb": 6, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.TrampolineHolder", "rp": "FSharp.Core_TrampolineHolder.html", "cl": 13, "ucl": 29, "cal": 42, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 2, "tb": 10, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Control.WebExtensions", "rp": "FSharp.Core_WebExtensions.html", "cl": 0, "ucl": 50, "cal": 50, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 8, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.AbstractClassAttribute", "rp": "FSharp.Core_AbstractClassAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.AllowNullLiteralAttribute", "rp": "FSharp.Core_AllowNullLiteralAttribute.html", "cl": 0, "ucl": 4, "cal": 4, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.AutoOpenAttribute", "rp": "FSharp.Core_AutoOpenAttribute.html", "cl": 0, "ucl": 4, "cal": 4, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.AutoSerializableAttribute", "rp": "FSharp.Core_AutoSerializableAttribute.html", "cl": 0, "ucl": 3, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.ByRefKinds", "rp": "FSharp.Core_ByRefKinds.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.ClassAttribute", "rp": "FSharp.Core_ClassAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CLIEventAttribute", "rp": "FSharp.Core_CLIEventAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CLIMutableAttribute", "rp": "FSharp.Core_CLIMutableAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.ComparisonConditionalOnAttribute", "rp": "FSharp.Core_ComparisonConditionalOnAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilationArgumentCountsAttribute", "rp": "FSharp.Core_CompilationArgumentCountsAttribute.html", "cl": 0, "ucl": 3, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilationMappingAttribute", "rp": "FSharp.Core_CompilationMappingAttribute.html", "cl": 8, "ucl": 7, "cal": 15, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilationRepresentationAttribute", "rp": "FSharp.Core_CompilationRepresentationAttribute.html", "cl": 0, "ucl": 3, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilationSourceNameAttribute", "rp": "FSharp.Core_CompilationSourceNameAttribute.html", "cl": 0, "ucl": 3, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompiledNameAttribute", "rp": "FSharp.Core_CompiledNameAttribute.html", "cl": 0, "ucl": 3, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilerMessageAttribute", "rp": "FSharp.Core_CompilerMessageAttribute.html", "cl": 0, "ucl": 8, "cal": 8, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilerServices.ArrayCollector<T>", "rp": "FSharp.Core_ArrayCollector_1.html", "cl": 0, "ucl": 35, "cal": 35, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 18, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilerServices.GeneratedSequenceBase<T>", "rp": "FSharp.Core_GeneratedSequenceBase_1.html", "cl": 0, "ucl": 28, "cal": 28, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 17, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilerServices.ListCollector<T>", "rp": "FSharp.Core_ListCollector_1.html", "cl": 0, "ucl": 28, "cal": 28, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 14, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilerServices.MeasureProduct<T1, T2>", "rp": "FSharp.Core_MeasureProduct_2.html", "cl": 0, "ucl": 1, "cal": 1, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilerServices.ResumableCode", "rp": "FSharp.Core_ResumableCode.html", "cl": 0, "ucl": 203, "cal": 203, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 16, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilerServices.ResumableStateMachine<T>", "rp": "FSharp.Core_ResumableStateMachine_1.html", "cl": 0, "ucl": 4, "cal": 4, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilerServices.ResumptionDynamicInfo<T>", "rp": "FSharp.Core_ResumptionDynamicInfo_1.html", "cl": 0, "ucl": 3, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers", "rp": "FSharp.Core_RuntimeHelpers.html", "cl": 57, "ucl": 53, "cal": 110, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 13, "tb": 32, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers", "rp": "FSharp.Core_StateMachineHelpers.html", "cl": 0, "ucl": 4, "cal": 4, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilerServices.TypeProviderAssemblyAttribute", "rp": "FSharp.Core_TypeProviderAssemblyAttribute.html", "cl": 0, "ucl": 4, "cal": 4, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilerServices.TypeProviderAttribute", "rp": "FSharp.Core_TypeProviderAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilerServices.TypeProviderConfig", "rp": "FSharp.Core_TypeProviderConfig.html", "cl": 0, "ucl": 16, "cal": 16, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilerServices.TypeProviderDefinitionLocationAttribute", "rp": "FSharp.Core_TypeProviderDefinitionLocationAttribute.html", "cl": 0, "ucl": 8, "cal": 8, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilerServices.TypeProviderEditorHideMethodsAttribute", "rp": "FSharp.Core_TypeProviderEditorHideMethodsAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CompilerServices.TypeProviderXmlDocAttribute", "rp": "FSharp.Core_TypeProviderXmlDocAttribute.html", "cl": 0, "ucl": 3, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CustomComparisonAttribute", "rp": "FSharp.Core_CustomComparisonAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CustomEqualityAttribute", "rp": "FSharp.Core_CustomEqualityAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.CustomOperationAttribute", "rp": "FSharp.Core_CustomOperationAttribute.html", "cl": 0, "ucl": 17, "cal": 17, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.DefaultAugmentationAttribute", "rp": "FSharp.Core_DefaultAugmentationAttribute.html", "cl": 0, "ucl": 3, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.DefaultValueAttribute", "rp": "FSharp.Core_DefaultValueAttribute.html", "cl": 0, "ucl": 4, "cal": 4, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.DetailedExceptions", "rp": "FSharp.Core_DetailedExceptions.html", "cl": 0, "ucl": 26, "cal": 26, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 6, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.EntryPointAttribute", "rp": "FSharp.Core_EntryPointAttribute.html", "cl": 2, "ucl": 0, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.EqualityConditionalOnAttribute", "rp": "FSharp.Core_EqualityConditionalOnAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.ExperimentalAttribute", "rp": "FSharp.Core_ExperimentalAttribute.html", "cl": 0, "ucl": 3, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.ExperimentalAttributeMessages", "rp": "FSharp.Core_ExperimentalAttributeMessages.html", "cl": 0, "ucl": 0, "cal": 0, "tl": 0, "ct": "MethodCoverage", "mc": 0, "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.ExtraTopLevelOperators", "rp": "FSharp.Core_ExtraTopLevelOperators.html", "cl": 0, "ucl": 115, "cal": 115, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 42, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.FSharpFunc<T1, T2>", "rp": "FSharp.Core_FSharpFunc_2.html", "cl": 1, "ucl": 6, "cal": 7, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.FSharpInterfaceDataVersionAttribute", "rp": "FSharp.Core_FSharpInterfaceDataVersionAttribute.html", "cl": 2, "ucl": 3, "cal": 5, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.FSharpOption<T>", "rp": "FSharp.Core_FSharpOption_1.html", "cl": 0, "ucl": 4, "cal": 4, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.FSharpRef<T>", "rp": "FSharp.Core_FSharpRef_1.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.FSharpTypeFunc", "rp": "FSharp.Core_FSharpTypeFunc.html", "cl": 0, "ucl": 1, "cal": 1, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.FSharpValueOption<T>", "rp": "FSharp.Core_FSharpValueOption_1.html", "cl": 0, "ucl": 8, "cal": 8, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.FuncConvert", "rp": "FSharp.Core_FuncConvert.html", "cl": 0, "ucl": 19, "cal": 19, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.GeneralizableValueAttribute", "rp": "FSharp.Core_GeneralizableValueAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.InlineIfLambdaAttribute", "rp": "FSharp.Core_InlineIfLambdaAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.InterfaceAttribute", "rp": "FSharp.Core_InterfaceAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.LanguagePrimitives", "rp": "FSharp.Core_LanguagePrimitives.html", "cl": 56, "ucl": 1201, "cal": 1257, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 39, "tb": 3092, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.LiteralAttribute", "rp": "FSharp.Core_LiteralAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.MatchFailureException", "rp": "FSharp.Core_MatchFailureException.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 18, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.MeasureAnnotatedAbbreviationAttribute", "rp": "FSharp.Core_MeasureAnnotatedAbbreviationAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.MeasureAttribute", "rp": "FSharp.Core_MeasureAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.NoComparisonAttribute", "rp": "FSharp.Core_NoComparisonAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.NoDynamicInvocationAttribute", "rp": "FSharp.Core_NoDynamicInvocationAttribute.html", "cl": 0, "ucl": 4, "cal": 4, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.NoEqualityAttribute", "rp": "FSharp.Core_NoEqualityAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.NumericLiterals", "rp": "FSharp.Core_NumericLiterals.html", "cl": 0, "ucl": 28, "cal": 28, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 14, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.Operators", "rp": "FSharp.Core_Operators.html", "cl": 1, "ucl": 627, "cal": 628, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 714, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.OptimizedClosures", "rp": "FSharp.Core_OptimizedClosures.html", "cl": 14, "ucl": 32, "cal": 46, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 1, "tb": 20, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.OptionalArgumentAttribute", "rp": "FSharp.Core_OptionalArgumentAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.OptionModule", "rp": "FSharp.Core_OptionModule.html", "cl": 6, "ucl": 20, "cal": 26, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 4, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.PrintfFormat<T1, T2, T3, T4>", "rp": "FSharp.Core_PrintfFormat_4.html", "cl": 1, "ucl": 5, "cal": 6, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.PrintfFormat<T1, T2, T3, T4, T5>", "rp": "FSharp.Core_PrintfFormat_5.html", "cl": 3, "ucl": 0, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.PrintfImpl", "rp": "FSharp.Core_PrintfImpl.html", "cl": 196, "ucl": 495, "cal": 691, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 67, "tb": 357, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.PrintfModule", "rp": "FSharp.Core_PrintfModule.html", "cl": 3, "ucl": 25, "cal": 28, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 1, "tb": 4, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.ProjectionParameterAttribute", "rp": "FSharp.Core_ProjectionParameterAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.ReferenceEqualityAttribute", "rp": "FSharp.Core_ReferenceEqualityAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.ReflectedDefinitionAttribute", "rp": "FSharp.Core_ReflectedDefinitionAttribute.html", "cl": 0, "ucl": 4, "cal": 4, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.RequireQualifiedAccessAttribute", "rp": "FSharp.Core_RequireQualifiedAccessAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.RequiresExplicitTypeArgumentsAttribute", "rp": "FSharp.Core_RequiresExplicitTypeArgumentsAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.ResultModule", "rp": "FSharp.Core_ResultModule.html", "cl": 0, "ucl": 3, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.SealedAttribute", "rp": "FSharp.Core_SealedAttribute.html", "cl": 0, "ucl": 4, "cal": 4, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.SR", "rp": "FSharp.Core_SR.html", "cl": 0, "ucl": 3, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.StringModule", "rp": "FSharp.Core_StringModule.html", "cl": 0, "ucl": 87, "cal": 87, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 70, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.StructAttribute", "rp": "FSharp.Core_StructAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.StructuralComparisonAttribute", "rp": "FSharp.Core_StructuralComparisonAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.StructuralEqualityAttribute", "rp": "FSharp.Core_StructuralEqualityAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.StructuredFormatDisplayAttribute", "rp": "FSharp.Core_StructuredFormatDisplayAttribute.html", "cl": 0, "ucl": 3, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.Unit", "rp": "FSharp.Core_Unit.html", "cl": 0, "ucl": 4, "cal": 4, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.UnverifiableAttribute", "rp": "FSharp.Core_UnverifiableAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.ValueAsStaticPropertyAttribute", "rp": "FSharp.Core_ValueAsStaticPropertyAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.ValueOption", "rp": "FSharp.Core_ValueOption.html", "cl": 0, "ucl": 26, "cal": 26, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 4, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Core.VolatileFieldAttribute", "rp": "FSharp.Core_VolatileFieldAttribute.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.ForwardDeclarations", "rp": "FSharp.Core_ForwardDeclarations.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.Helpers", "rp": "FSharp.Core_Helpers.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.NullableModule", "rp": "FSharp.Core_NullableModule.html", "cl": 0, "ucl": 21, "cal": 21, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 62, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.NullableOperators", "rp": "FSharp.Core_NullableOperators.html", "cl": 0, "ucl": 33, "cal": 33, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 104, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.QueryBuilder", "rp": "FSharp.Core_QueryBuilder.html", "cl": 1, "ucl": 79, "cal": 80, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 32, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.QueryModule", "rp": "FSharp.Core_QueryModule.html", "cl": 0, "ucl": 609, "cal": 609, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 940, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.QueryRunExtensions.HighPriority", "rp": "FSharp.Core_HighPriority.2.html", "cl": 0, "ucl": 1, "cal": 1, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.QueryRunExtensions.LowPriority", "rp": "FSharp.Core_LowPriority.2.html", "cl": 0, "ucl": 1, "cal": 1, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.QuerySource<T1, T2>", "rp": "FSharp.Core_QuerySource_2.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.RuntimeHelpers.Adapters", "rp": "FSharp.Core_Adapters.html", "cl": 0, "ucl": 78, "cal": 78, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 62, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.RuntimeHelpers.AnonymousObject<T>", "rp": "FSharp.Core_AnonymousObject_1.html", "cl": 0, "ucl": 2, "cal": 2, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.RuntimeHelpers.AnonymousObject<T1, T2>", "rp": "FSharp.Core_AnonymousObject_2.html", "cl": 0, "ucl": 3, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.RuntimeHelpers.AnonymousObject<T1, T2, T3>", "rp": "FSharp.Core_AnonymousObject_3.html", "cl": 0, "ucl": 4, "cal": 4, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.RuntimeHelpers.AnonymousObject<T1, T2, T3, T4>", "rp": "FSharp.Core_AnonymousObject_4.html", "cl": 0, "ucl": 5, "cal": 5, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.RuntimeHelpers.AnonymousObject<T1, T2, T3, T4, T5>", "rp": "FSharp.Core_AnonymousObject_5.html", "cl": 0, "ucl": 6, "cal": 6, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.RuntimeHelpers.AnonymousObject<T1, T2, T3, T4, T5, T6>", "rp": "FSharp.Core_AnonymousObject_6.html", "cl": 0, "ucl": 7, "cal": 7, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.RuntimeHelpers.AnonymousObject<T1, T2, T3, T4, T5, T6, T7>", "rp": "FSharp.Core_AnonymousObject_7.html", "cl": 0, "ucl": 8, "cal": 8, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.RuntimeHelpers.AnonymousObject<T1, T2, T3, T4, T5, T6, T7, T8>", "rp": "FSharp.Core_AnonymousObject_8.html", "cl": 0, "ucl": 9, "cal": 9, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.RuntimeHelpers.Grouping<T1, T2>", "rp": "FSharp.Core_Grouping_2.html", "cl": 0, "ucl": 4, "cal": 4, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter", "rp": "FSharp.Core_LeafExpressionConverter.html", "cl": 0, "ucl": 306, "cal": 306, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 668, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.NativeInterop.NativePtrModule", "rp": "FSharp.Core_NativePtrModule.html", "cl": 0, "ucl": 19, "cal": 19, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Primitives.Basics.Array", "rp": "FSharp.Core_Array.html", "cl": 0, "ucl": 124, "cal": 124, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 96, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Primitives.Basics.List", "rp": "FSharp.Core_List.html", "cl": 16, "ucl": 474, "cal": 490, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 3, "tb": 146, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Primitives.Basics.Seq", "rp": "FSharp.Core_Seq.html", "cl": 0, "ucl": 11, "cal": 11, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 12, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Quotations.DerivedPatternsModule", "rp": "FSharp.Core_DerivedPatternsModule.html", "cl": 0, "ucl": 58, "cal": 58, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 22, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Quotations.ExprShapeModule", "rp": "FSharp.Core_ExprShapeModule.html", "cl": 0, "ucl": 48, "cal": 48, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 177, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Quotations.FSharpExpr", "rp": "FSharp.Core_FSharpExpr.html", "cl": 0, "ucl": 127, "cal": 127, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 167, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Quotations.FSharpExpr<T>", "rp": "FSharp.Core_FSharpExpr_1.html", "cl": 0, "ucl": 3, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Quotations.FSharpVar", "rp": "FSharp.Core_FSharpVar.html", "cl": 0, "ucl": 32, "cal": 32, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 12, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Quotations.Helpers", "rp": "FSharp.Core_Helpers.2.html", "cl": 0, "ucl": 19, "cal": 19, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 10, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Quotations.PatternsModule", "rp": "FSharp.Core_PatternsModule.html", "cl": 0, "ucl": 767, "cal": 767, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 546, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Reflection.DynamicFunction<T1, T2>", "rp": "FSharp.Core_DynamicFunction_2.html", "cl": 0, "ucl": 3, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Reflection.FSharpReflectionExtensions", "rp": "FSharp.Core_FSharpReflectionExtensions.html", "cl": 0, "ucl": 38, "cal": 38, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Reflection.FSharpType", "rp": "FSharp.Core_FSharpType.html", "cl": 0, "ucl": 34, "cal": 34, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 6, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Reflection.FSharpValue", "rp": "FSharp.Core_FSharpValue.html", "cl": 0, "ucl": 78, "cal": 78, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 14, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Reflection.Impl", "rp": "FSharp.Core_Impl.html", "cl": 0, "ucl": 411, "cal": 411, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 261, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Reflection.ReflectionUtils", "rp": "FSharp.Core_ReflectionUtils.html", "cl": 0, "ucl": 3, "cal": 3, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 2, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Reflection.UnionCaseInfo", "rp": "FSharp.Core_UnionCaseInfo.html", "cl": 0, "ucl": 18, "cal": 18, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 2, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "rp": "FSharp.Core_Display.html", "cl": 0, "ucl": 370, "cal": 370, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 241, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Text.StructuredPrintfImpl.FormatOptions", "rp": "FSharp.Core_FormatOptions.html", "cl": 0, "ucl": 15, "cal": 15, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Text.StructuredPrintfImpl.Layout", "rp": "FSharp.Core_Layout.html", "cl": 0, "ucl": 9, "cal": 9, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 2, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Text.StructuredPrintfImpl.LayoutModule", "rp": "FSharp.Core_LayoutModule.html", "cl": 0, "ucl": 53, "cal": 53, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 14, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Text.StructuredPrintfImpl.ReflectUtils", "rp": "FSharp.Core_ReflectUtils.html", "cl": 0, "ucl": 47, "cal": 47, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 38, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Text.StructuredPrintfImpl.TaggedText", "rp": "FSharp.Core_TaggedText.html", "cl": 0, "ucl": 4, "cal": 4, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "Microsoft.FSharp.Text.StructuredPrintfImpl.TaggedTextModule", "rp": "FSharp.Core_TaggedTextModule.html", "cl": 0, "ucl": 8, "cal": 8, "tl": 0, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
    ]},
  {
    "name": "ImageProcessing",
    "classes": [
      { "name": "ImageProcessing.ImageProcessing", "rp": "ImageProcessing_ImageProcessing.html", "cl": 0, "ucl": 93, "cal": 93, "tl": 169, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 26, "lch": [], "bch": [], "hc": [] },
      { "name": "ImageProcessing.Main", "rp": "ImageProcessing_Main.html", "cl": 0, "ucl": 1, "cal": 1, "tl": 37, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 0, "lch": [], "bch": [], "hc": [] },
      { "name": "ImageProcessing.Streaming", "rp": "ImageProcessing_Streaming.html", "cl": 0, "ucl": 52, "cal": 52, "tl": 76, "ct": "LineCoverage", "mc": "-", "cb": 0, "tb": 10, "lch": [], "bch": [], "hc": [] },
    ]},
];

var historicCoverageExecutionTimes = [];

var riskHotspotMetrics = [
      { "name": "Cyclomatic complexity", "explanationUrl": "https://en.wikipedia.org/wiki/Cyclomatic_complexity" },
      { "name": "NPath complexity", "explanationUrl": "https://modess.io/npath-complexity-cyclomatic-complexity-explained" },
      { "name": "Crap Score", "explanationUrl": "https://googletesting.blogspot.de/2011/02/this-code-is-crap.html" },
];

var riskHotspots = [
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.ArrayModule/Filter::populateMask(Microsoft.FSharp.Core.FSharpFunc`2<a,System.Boolean>,a[],System.UInt32[])", "methodShortName": "populateMask(...)", "fileIndex": 0, "line": 526,
    "metrics": [
      { "value": 34, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 1190, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.ArrayModule/Filter::populateDstViaMask(a[],System.UInt32[],a[])", "methodShortName": "populateDstViaMask(...)", "fileIndex": 0, "line": 594,
    "metrics": [
      { "value": 38, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 1482, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::AdditionDynamic(T1,T2)", "methodShortName": "AdditionDynamic(...)", "fileIndex": 0, "line": 2591,
    "metrics": [
      { "value": 46, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 2162, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::SubtractionDynamic(T1,T2)", "methodShortName": "SubtractionDynamic(...)", "fileIndex": 0, "line": 2610,
    "metrics": [
      { "value": 40, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 1640, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::MultiplyDynamic(T1,T2)", "methodShortName": "MultiplyDynamic(...)", "fileIndex": 0, "line": 2627,
    "metrics": [
      { "value": 40, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 1640, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::DivisionDynamic(T1,T2)", "methodShortName": "DivisionDynamic(...)", "fileIndex": 0, "line": 2644,
    "metrics": [
      { "value": 40, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 1640, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::ModulusDynamic(T1,T2)", "methodShortName": "ModulusDynamic(...)", "fileIndex": 0, "line": 2661,
    "metrics": [
      { "value": 40, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 1640, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::CheckedAdditionDynamic(T1,T2)", "methodShortName": "CheckedAdditionDynamic(...)", "fileIndex": 0, "line": 2690,
    "metrics": [
      { "value": 46, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 2162, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::CheckedSubtractionDynamic(T1,T2)", "methodShortName": "CheckedSubtractionDynamic(...)", "fileIndex": 0, "line": 2709,
    "metrics": [
      { "value": 40, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 1640, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::CheckedMultiplyDynamic(T1,T2)", "methodShortName": "CheckedMultiplyDynamic(...)", "fileIndex": 0, "line": 2726,
    "metrics": [
      { "value": 40, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 1640, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::ExplicitDynamic(T)", "methodShortName": "ExplicitDynamic(...)", "fileIndex": 0, "line": 2839,
    "metrics": [
      { "value": 215, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 46440, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::LessThanDynamic(T1,T2)", "methodShortName": "LessThanDynamic(...)", "fileIndex": 0, "line": 3070,
    "metrics": [
      { "value": 46, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 2162, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::GreaterThanDynamic(T1,T2)", "methodShortName": "GreaterThanDynamic(...)", "fileIndex": 0, "line": 3089,
    "metrics": [
      { "value": 46, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 2162, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::LessThanOrEqualDynamic(T1,T2)", "methodShortName": "LessThanOrEqualDynamic(...)", "fileIndex": 0, "line": 3108,
    "metrics": [
      { "value": 46, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 2162, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::GreaterThanOrEqualDynamic(T1,T2)", "methodShortName": "GreaterThanOrEqualDynamic(...)", "fileIndex": 0, "line": 3127,
    "metrics": [
      { "value": 46, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 2162, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::EqualityDynamic(T1,T2)", "methodShortName": "EqualityDynamic(...)", "fileIndex": 0, "line": 3146,
    "metrics": [
      { "value": 46, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 2162, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::InequalityDynamic(T1,T2)", "methodShortName": "InequalityDynamic(...)", "fileIndex": 0, "line": 3165,
    "metrics": [
      { "value": 46, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 2162, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Boolean Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::GenericEqualityObj$cont@1366(System.Boolean,System.Collections.IEqualityComparer,System.Object,System.Object,System.Array,Microsoft.FSharp.Core.Unit)", "methodShortName": "GenericEqualityObj$cont@1366(...)", "fileIndex": 0, "line": 1368,
    "metrics": [
      { "value": 383, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 147072, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "System.Tuple`2<Microsoft.FSharp.Linq.QueryModule/TransInnerResult,Microsoft.FSharp.Linq.RuntimeHelpers.Adapters/ConversionDescription> Microsoft.FSharp.Linq.QueryModule::TransInner(Microsoft.FSharp.Linq.QueryModule/CanEliminate,System.Boolean,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "TransInner(...)", "fileIndex": 0, "line": 1278,
    "metrics": [
      { "value": 180, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 32580, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Linq.QueryModule::TransNestedOuter(Microsoft.FSharp.Linq.QueryModule/CanEliminate,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "TransNestedOuter(...)", "fileIndex": 0, "line": 1698,
    "metrics": [
      { "value": 102, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 10506, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "System.Object Microsoft.FSharp.Linq.QueryModule::EvalNonNestedOuter(Microsoft.FSharp.Linq.QueryModule/CanEliminate,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "EvalNonNestedOuter(...)", "fileIndex": 0, "line": 1833,
    "metrics": [
      { "value": 102, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 10506, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter", "reportPath": "FSharp.Core_LeafExpressionConverter.html", "methodName": "System.Linq.Expressions.Expression Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter::ConvExprToLinqInContext(Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter/ConvEnv,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "ConvExprToLinqInContext(...)", "fileIndex": 0, "line": 276,
    "metrics": [
      { "value": 325, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 105950, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.ExprShapeModule", "reportPath": "FSharp.Core_ExprShapeModule.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Quotations.ExprShapeModule::RebuildShapeCombination(System.Object,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>)", "methodShortName": "RebuildShapeCombination(...)", "fileIndex": 0, "line": 2255,
    "metrics": [
      { "value": 84, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 7140, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.FSharpExpr", "reportPath": "FSharp.Core_FSharpExpr.html", "methodName": "Microsoft.FSharp.Text.StructuredPrintfImpl.Layout Microsoft.FSharp.Quotations.FSharpExpr::GetLayout(System.Boolean)", "methodShortName": "GetLayout(...)", "fileIndex": 0, "line": 251,
    "metrics": [
      { "value": 98, "exceeded": true },
      { "value": 2147483647, "exceeded": true },
      { "value": 9702, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::LeftShiftDynamic(T1,T2)", "methodShortName": "LeftShiftDynamic(...)", "fileIndex": 0, "line": 2755,
    "metrics": [
      { "value": 31, "exceeded": true },
      { "value": 60466176, "exceeded": true },
      { "value": 992, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::RightShiftDynamic(T1,T2)", "methodShortName": "RightShiftDynamic(...)", "fileIndex": 0, "line": 2769,
    "metrics": [
      { "value": 31, "exceeded": true },
      { "value": 60466176, "exceeded": true },
      { "value": 992, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::BitwiseAndDynamic(T1,T2)", "methodShortName": "BitwiseAndDynamic(...)", "fileIndex": 0, "line": 2783,
    "metrics": [
      { "value": 31, "exceeded": true },
      { "value": 60466176, "exceeded": true },
      { "value": 992, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::BitwiseOrDynamic(T1,T2)", "methodShortName": "BitwiseOrDynamic(...)", "fileIndex": 0, "line": 2797,
    "metrics": [
      { "value": 31, "exceeded": true },
      { "value": 60466176, "exceeded": true },
      { "value": 992, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::ExclusiveOrDynamic(T1,T2)", "methodShortName": "ExclusiveOrDynamic(...)", "fileIndex": 0, "line": 2811,
    "metrics": [
      { "value": 31, "exceeded": true },
      { "value": 60466176, "exceeded": true },
      { "value": 992, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "System.Type Microsoft.FSharp.Quotations.PatternsModule::typeOf(T)", "methodShortName": "typeOf(...)", "fileIndex": 0, "line": 670,
    "metrics": [
      { "value": 66, "exceeded": true },
      { "value": 38043648, "exceeded": true },
      { "value": 4422, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::LogicalNotDynamic(T)", "methodShortName": "LogicalNotDynamic(...)", "fileIndex": 0, "line": 2825,
    "metrics": [
      { "value": 21, "exceeded": false },
      { "value": 1048576, "exceeded": true },
      { "value": 462, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::UnaryNegationDynamic(T)", "methodShortName": "UnaryNegationDynamic(...)", "fileIndex": 0, "line": 2678,
    "metrics": [
      { "value": 17, "exceeded": false },
      { "value": 65536, "exceeded": true },
      { "value": 306, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives::CheckedUnaryNegationDynamic(T)", "methodShortName": "CheckedUnaryNegationDynamic(...)", "fileIndex": 0, "line": 2743,
    "metrics": [
      { "value": 17, "exceeded": false },
      { "value": 65536, "exceeded": true },
      { "value": 306, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[0...,0...,0...,0...] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice4D(T[0...,0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "GetArraySlice4D(...)", "fileIndex": 0, "line": 5753,
    "metrics": [
      { "value": 29, "exceeded": false },
      { "value": 65536, "exceeded": true },
      { "value": 870, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice4D(T[0...,0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,T[0...,0...,0...,0...])", "methodShortName": "SetArraySlice4D(...)", "fileIndex": 0, "line": 5877,
    "metrics": [
      { "value": 17, "exceeded": false },
      { "value": 65536, "exceeded": true },
      { "value": 306, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr> Microsoft.FSharp.Linq.QueryModule::|AnyNestedQuery|_|(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "|AnyNestedQuery|_|(...)", "fileIndex": 0, "line": 1220,
    "metrics": [
      { "value": 223, "exceeded": true },
      { "value": 0, "exceeded": false },
      { "value": 49952, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Int32 Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::GenericCompare(Microsoft.FSharp.Core.LanguagePrimitives/HashCompare/GenericComparer,System.Object,System.Object)", "methodShortName": "GenericCompare(...)", "fileIndex": 0, "line": 876,
    "metrics": [
      { "value": 57, "exceeded": true },
      { "value": 10240, "exceeded": true },
      { "value": 2474.81, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "reportPath": "FSharp.Core_Display.html", "methodName": "Microsoft.FSharp.Text.StructuredPrintfImpl.TaggedText Microsoft.FSharp.Text.StructuredPrintfImpl.Display::leafFormatter(Microsoft.FSharp.Text.StructuredPrintfImpl.FormatOptions,System.Object)", "methodShortName": "leafFormatter(...)", "fileIndex": 0, "line": 1279,
    "metrics": [
      { "value": 32, "exceeded": true },
      { "value": 10240, "exceeded": true },
      { "value": 1056, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "System.Int32 Microsoft.FSharp.Core.PrintfImpl/Step::BlockCount(Microsoft.FSharp.Core.PrintfImpl/Step[])", "methodShortName": "BlockCount(...)", "fileIndex": 0, "line": 271,
    "metrics": [
      { "value": 19, "exceeded": false },
      { "value": 9216, "exceeded": true },
      { "value": 131.21, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SetTreeModule", "reportPath": "FSharp.Core_SetTreeModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SetTreeModule::compareStacks(System.Collections.Generic.IComparer`1<T>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Collections.SetTree`1<T>>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Collections.SetTree`1<T>>)", "methodShortName": "compareStacks(...)", "fileIndex": 0, "line": 481,
    "metrics": [
      { "value": 17, "exceeded": false },
      { "value": 8192, "exceeded": true },
      { "value": 306, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Array2DModule", "reportPath": "FSharp.Core_Array2DModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.Array2DModule::CopyTo(T[0...,0...],System.Int32,System.Int32,T[0...,0...],System.Int32,System.Int32,System.Int32,System.Int32)", "methodShortName": "CopyTo(...)", "fileIndex": 0, "line": 140,
    "metrics": [
      { "value": 15, "exceeded": false },
      { "value": 4096, "exceeded": true },
      { "value": 240, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Int32 Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::GenericComparisonArbArrayWithComparer(Microsoft.FSharp.Core.LanguagePrimitives/HashCompare/GenericComparer,System.Array,System.Array)", "methodShortName": "GenericComparisonArbArrayWithComparer(...)", "fileIndex": 0, "line": 934,
    "metrics": [
      { "value": 20, "exceeded": false },
      { "value": 4096, "exceeded": true },
      { "value": 292.59, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Boolean Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::GenericEqualityArbArray(System.Boolean,System.Collections.IEqualityComparer,System.Array,System.Array)", "methodShortName": "GenericEqualityArbArray(...)", "fileIndex": 0, "line": 1390,
    "metrics": [
      { "value": 13, "exceeded": false },
      { "value": 4096, "exceeded": true },
      { "value": 182, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[0...,0...,0...] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice3D(T[0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "GetArraySlice3D(...)", "fileIndex": 0, "line": 5628,
    "metrics": [
      { "value": 22, "exceeded": false },
      { "value": 4096, "exceeded": true },
      { "value": 506, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice3D(T[0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,T[0...,0...,0...])", "methodShortName": "SetArraySlice3D(...)", "fileIndex": 0, "line": 5692,
    "metrics": [
      { "value": 13, "exceeded": false },
      { "value": 4096, "exceeded": true },
      { "value": 182, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[0...,0...,0...] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice4DFixedSingle1(T[0...,0...,0...,0...],System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "GetArraySlice4DFixedSingle1(...)", "fileIndex": 0, "line": 5794,
    "metrics": [
      { "value": 22, "exceeded": false },
      { "value": 4096, "exceeded": true },
      { "value": 506, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[0...,0...,0...] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice4DFixedSingle2(T[0...,0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "GetArraySlice4DFixedSingle2(...)", "fileIndex": 0, "line": 5797,
    "metrics": [
      { "value": 22, "exceeded": false },
      { "value": 4096, "exceeded": true },
      { "value": 506, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[0...,0...,0...] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice4DFixedSingle3(T[0...,0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "GetArraySlice4DFixedSingle3(...)", "fileIndex": 0, "line": 5800,
    "metrics": [
      { "value": 22, "exceeded": false },
      { "value": 4096, "exceeded": true },
      { "value": 506, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[0...,0...,0...] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice4DFixedSingle4(T[0...,0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32)", "methodShortName": "GetArraySlice4DFixedSingle4(...)", "fileIndex": 0, "line": 5803,
    "metrics": [
      { "value": 22, "exceeded": false },
      { "value": 4096, "exceeded": true },
      { "value": 506, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice4DFixedSingle1(T[0...,0...,0...,0...],System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,T[0...,0...,0...])", "methodShortName": "SetArraySlice4DFixedSingle1(...)", "fileIndex": 0, "line": 5916,
    "metrics": [
      { "value": 19, "exceeded": false },
      { "value": 4096, "exceeded": true },
      { "value": 380, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice4DFixedSingle2(T[0...,0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,T[0...,0...,0...])", "methodShortName": "SetArraySlice4DFixedSingle2(...)", "fileIndex": 0, "line": 5919,
    "metrics": [
      { "value": 19, "exceeded": false },
      { "value": 4096, "exceeded": true },
      { "value": 380, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice4DFixedSingle3(T[0...,0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,T[0...,0...,0...])", "methodShortName": "SetArraySlice4DFixedSingle3(...)", "fileIndex": 0, "line": 5922,
    "metrics": [
      { "value": 19, "exceeded": false },
      { "value": 4096, "exceeded": true },
      { "value": 380, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice4DFixedSingle4(T[0...,0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,T[0...,0...,0...])", "methodShortName": "SetArraySlice4DFixedSingle4(...)", "fileIndex": 0, "line": 5925,
    "metrics": [
      { "value": 19, "exceeded": false },
      { "value": 4096, "exceeded": true },
      { "value": 380, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Quotations.PatternsModule/ModuleDefinitionBindingResult`2<Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Quotations.ExprConstInfo>,Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Quotations.ExprConstInfo>>> Microsoft.FSharp.Quotations.PatternsModule::u_constSpec(Microsoft.FSharp.Quotations.PatternsModule/SimpleUnpickle/InputState)", "methodShortName": "u_constSpec(...)", "fileIndex": 0, "line": 1632,
    "metrics": [
      { "value": 54, "exceeded": true },
      { "value": 800, "exceeded": true },
      { "value": 2970, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr> Microsoft.FSharp.Quotations.PatternsModule::tryGetReflectedDefinition(System.Reflection.MethodBase,System.Type[])", "methodShortName": "tryGetReflectedDefinition(...)", "fileIndex": 0, "line": 1817,
    "metrics": [
      { "value": 19, "exceeded": false },
      { "value": 2048, "exceeded": true },
      { "value": 380, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Type Microsoft.FSharp.Reflection.Impl::mkTupleType(System.Boolean,System.Reflection.Assembly,System.Type[])", "methodShortName": "mkTupleType(...)", "fileIndex": 0, "line": 588,
    "metrics": [
      { "value": 21, "exceeded": false },
      { "value": 2048, "exceeded": true },
      { "value": 462, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "reportPath": "FSharp.Core_Display.html", "methodName": "Microsoft.FSharp.Text.StructuredPrintfImpl.Layout Microsoft.FSharp.Text.StructuredPrintfImpl.Display/ObjectGraphFormatter::reprL(Microsoft.FSharp.Text.StructuredPrintfImpl.Display/ShowMode,System.Int32,Microsoft.FSharp.Text.StructuredPrintfImpl.Display/Precedence,Microsoft.FSharp.Text.StructuredPrintfImpl.ReflectUtils/ValueInfo,System.Object)", "methodShortName": "reprL(...)", "fileIndex": 0, "line": 1218,
    "metrics": [
      { "value": 20, "exceeded": false },
      { "value": 1792, "exceeded": true },
      { "value": 420, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr> Microsoft.FSharp.Linq.QueryModule::|EnumerableNestedQuery|_|(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "|EnumerableNestedQuery|_|(...)", "fileIndex": 0, "line": 1226,
    "metrics": [
      { "value": 37, "exceeded": true },
      { "value": 0, "exceeded": false },
      { "value": 1406, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "TResult[] Microsoft.FSharp.Collections.ArrayModule::Choose(Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.FSharpOption`1<TResult>>,T[])", "methodShortName": "Choose(...)", "fileIndex": 0, "line": 473,
    "metrics": [
      { "value": 14, "exceeded": false },
      { "value": 1024, "exceeded": true },
      { "value": 210, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Linq.QueryModule::Make@575-3(System.Boolean,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>,Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>,Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>,Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>,Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>,Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>,Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>,Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>,Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>,Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>,Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`3<Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>,Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Quotations.FSharpExpr,System.Boolean,Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpVar,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "Make@575-3(...)", "fileIndex": 0, "line": 576,
    "metrics": [
      { "value": 11, "exceeded": false },
      { "value": 1024, "exceeded": true },
      { "value": 132, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "reportPath": "FSharp.Core_Display.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Text.StructuredPrintfImpl.Layout> Microsoft.FSharp.Text.StructuredPrintfImpl.Display::buildObjMessageL@951(Microsoft.FSharp.Text.StructuredPrintfImpl.Display/ObjectGraphFormatter,Microsoft.FSharp.Text.StructuredPrintfImpl.Display/ShowMode,System.Type,System.Int32,System.Object,System.String,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Text.StructuredPrintfImpl.Layout>)", "methodShortName": "buildObjMessageL@951(...)", "fileIndex": 0, "line": 960,
    "metrics": [
      { "value": 13, "exceeded": false },
      { "value": 1024, "exceeded": true },
      { "value": 182, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Collections.Generic.IComparer`1<T> Microsoft.FSharp.Core.LanguagePrimitives::.cctor$cont@2181-2(System.Type,Microsoft.FSharp.Core.Unit)", "methodShortName": ".cctor$cont@2181-2(...)", "fileIndex": 0, "line": 2189,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 512, "exceeded": true },
      { "value": 24.58, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Void Microsoft.FSharp.Core.LanguagePrimitives/FastGenericComparerTable`1::.cctor()", "methodShortName": ".cctor()", "fileIndex": 0, "line": 2180,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 512, "exceeded": true },
      { "value": 15.99, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "reportPath": "FSharp.Core_Display.html", "methodName": "Microsoft.FSharp.Text.StructuredPrintfImpl.Layout Microsoft.FSharp.Text.StructuredPrintfImpl.Display/ObjectGraphFormatter::objL(Microsoft.FSharp.Text.StructuredPrintfImpl.Display/ShowMode,System.Int32,Microsoft.FSharp.Text.StructuredPrintfImpl.Display/Precedence,System.Object,System.Type)", "methodShortName": "objL(...)", "fileIndex": 0, "line": 893,
    "metrics": [
      { "value": 12, "exceeded": false },
      { "value": 512, "exceeded": true },
      { "value": 156, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Boolean Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::GenericEqualityObj(System.Boolean,System.Collections.IEqualityComparer,System.Object,System.Object)", "methodShortName": "GenericEqualityObj(...)", "fileIndex": 0, "line": 1360,
    "metrics": [
      { "value": 28, "exceeded": false },
      { "value": 36, "exceeded": false },
      { "value": 503.05, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.RuntimeHelpers.Adapters", "reportPath": "FSharp.Core_Adapters.html", "methodName": "System.Type Microsoft.FSharp.Linq.RuntimeHelpers.Adapters::ConvImmutableTypeToMutableType(Microsoft.FSharp.Linq.RuntimeHelpers.Adapters/ConversionDescription,System.Type)", "methodShortName": "ConvImmutableTypeToMutableType(...)", "fileIndex": 0, "line": 195,
    "metrics": [
      { "value": 20, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 420, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "System.Reflection.MethodBase Microsoft.FSharp.Quotations.PatternsModule::u_MethodBase(Microsoft.FSharp.Quotations.PatternsModule/SimpleUnpickle/InputState)", "methodShortName": "u_MethodBase(...)", "fileIndex": 0, "line": 1592,
    "metrics": [
      { "value": 14, "exceeded": false },
      { "value": 300, "exceeded": true },
      { "value": 210, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule/InsertManyAt@1547::GenerateNext(System.Collections.Generic.IEnumerable`1<T>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 1, "line": 1547,
    "metrics": [
      { "value": 16, "exceeded": false },
      { "value": 128, "exceeded": false },
      { "value": 272, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.DerivedPatternsModule", "reportPath": "FSharp.Core_DerivedPatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Decimal> Microsoft.FSharp.Quotations.DerivedPatternsModule::DecimalPattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "DecimalPattern(...)", "fileIndex": 0, "line": 2233,
    "metrics": [
      { "value": 16, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 272, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Array4DModule", "reportPath": "FSharp.Core_Array4DModule.html", "methodName": "T[0...,0...,0...,0...] Microsoft.FSharp.Collections.Array4DModule::Create(System.Int32,System.Int32,System.Int32,System.Int32,T)", "methodShortName": "Create(...)", "fileIndex": 0, "line": 137,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Array4DModule", "reportPath": "FSharp.Core_Array4DModule.html", "methodName": "T[0...,0...,0...,0...] Microsoft.FSharp.Collections.Array4DModule::Initialize(System.Int32,System.Int32,System.Int32,System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,T>>>>)", "methodShortName": "Initialize(...)", "fileIndex": 0, "line": 147,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.MapTreeModule", "reportPath": "FSharp.Core_MapTreeModule.html", "methodName": "Microsoft.FSharp.Collections.MapTree`2<TKey,TValue> Microsoft.FSharp.Collections.MapTreeModule::change(System.Collections.Generic.IComparer`1<TKey>,TKey,Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Core.FSharpOption`1<TValue>,Microsoft.FSharp.Core.FSharpOption`1<TValue>>,Microsoft.FSharp.Collections.MapTree`2<TKey,TValue>)", "methodShortName": "change(...)", "fileIndex": 0, "line": 237,
    "metrics": [
      { "value": 14, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 210, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.MapTreeModule", "reportPath": "FSharp.Core_MapTreeModule.html", "methodName": "b Microsoft.FSharp.Collections.MapTreeModule::foldFromTo@402(System.Collections.Generic.IComparer`1<TKey>,TKey,TKey,Microsoft.FSharp.Core.OptimizedClosures/FSharpFunc`4<TKey,TValue,b,b>,Microsoft.FSharp.Collections.MapTree`2<TKey,TValue>,b)", "methodShortName": "foldFromTo@402(...)", "fileIndex": 0, "line": 403,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "T Microsoft.FSharp.Core.LanguagePrimitives::.cctor$cont@2447-5(System.Type,Microsoft.FSharp.Core.Unit)", "methodShortName": ".cctor$cont@2447-5(...)", "fileIndex": 0, "line": 2447,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "T Microsoft.FSharp.Core.LanguagePrimitives::.cctor$cont@2471-6(System.Type,Microsoft.FSharp.Core.Unit)", "methodShortName": ".cctor$cont@2471-6(...)", "fileIndex": 0, "line": 2471,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[0...,0...] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice2D(T[0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "GetArraySlice2D(...)", "fileIndex": 0, "line": 5576,
    "metrics": [
      { "value": 15, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 240, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice2D(T[0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,T[0...,0...])", "methodShortName": "SetArraySlice2D(...)", "fileIndex": 0, "line": 5619,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[0...,0...] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice3DFixedSingle1(T[0...,0...,0...],System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "GetArraySlice3DFixedSingle1(...)", "fileIndex": 0, "line": 5661,
    "metrics": [
      { "value": 15, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 240, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[0...,0...] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice3DFixedSingle2(T[0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "GetArraySlice3DFixedSingle2(...)", "fileIndex": 0, "line": 5663,
    "metrics": [
      { "value": 15, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 240, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[0...,0...] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice3DFixedSingle3(T[0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32)", "methodShortName": "GetArraySlice3DFixedSingle3(...)", "fileIndex": 0, "line": 5665,
    "metrics": [
      { "value": 15, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 240, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice3DFixedSingle1(T[0...,0...,0...],System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,T[0...,0...])", "methodShortName": "SetArraySlice3DFixedSingle1(...)", "fileIndex": 0, "line": 5722,
    "metrics": [
      { "value": 13, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 182, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice3DFixedSingle2(T[0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,T[0...,0...])", "methodShortName": "SetArraySlice3DFixedSingle2(...)", "fileIndex": 0, "line": 5725,
    "metrics": [
      { "value": 13, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 182, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice3DFixedSingle3(T[0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,T[0...,0...])", "methodShortName": "SetArraySlice3DFixedSingle3(...)", "fileIndex": 0, "line": 5728,
    "metrics": [
      { "value": 13, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 182, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[0...,0...] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice4DFixedDouble1(T[0...,0...,0...,0...],System.Int32,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "GetArraySlice4DFixedDouble1(...)", "fileIndex": 0, "line": 5830,
    "metrics": [
      { "value": 15, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 240, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[0...,0...] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice4DFixedDouble2(T[0...,0...,0...,0...],System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "GetArraySlice4DFixedDouble2(...)", "fileIndex": 0, "line": 5833,
    "metrics": [
      { "value": 15, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 240, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[0...,0...] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice4DFixedDouble3(T[0...,0...,0...,0...],System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32)", "methodShortName": "GetArraySlice4DFixedDouble3(...)", "fileIndex": 0, "line": 5836,
    "metrics": [
      { "value": 15, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 240, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[0...,0...] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice4DFixedDouble4(T[0...,0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "GetArraySlice4DFixedDouble4(...)", "fileIndex": 0, "line": 5839,
    "metrics": [
      { "value": 15, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 240, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[0...,0...] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice4DFixedDouble5(T[0...,0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32)", "methodShortName": "GetArraySlice4DFixedDouble5(...)", "fileIndex": 0, "line": 5842,
    "metrics": [
      { "value": 15, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 240, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[0...,0...] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice4DFixedDouble6(T[0...,0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,System.Int32)", "methodShortName": "GetArraySlice4DFixedDouble6(...)", "fileIndex": 0, "line": 5845,
    "metrics": [
      { "value": 15, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 240, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice4DFixedDouble1(T[0...,0...,0...,0...],System.Int32,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,T[0...,0...])", "methodShortName": "SetArraySlice4DFixedDouble1(...)", "fileIndex": 0, "line": 5950,
    "metrics": [
      { "value": 13, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 182, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice4DFixedDouble2(T[0...,0...,0...,0...],System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,T[0...,0...])", "methodShortName": "SetArraySlice4DFixedDouble2(...)", "fileIndex": 0, "line": 5953,
    "metrics": [
      { "value": 13, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 182, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice4DFixedDouble3(T[0...,0...,0...,0...],System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,T[0...,0...])", "methodShortName": "SetArraySlice4DFixedDouble3(...)", "fileIndex": 0, "line": 5956,
    "metrics": [
      { "value": 13, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 182, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice4DFixedDouble4(T[0...,0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,T[0...,0...])", "methodShortName": "SetArraySlice4DFixedDouble4(...)", "fileIndex": 0, "line": 5959,
    "metrics": [
      { "value": 13, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 182, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice4DFixedDouble5(T[0...,0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,T[0...,0...])", "methodShortName": "SetArraySlice4DFixedDouble5(...)", "fileIndex": 0, "line": 5962,
    "metrics": [
      { "value": 13, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 182, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice4DFixedDouble6(T[0...,0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,System.Int32,T[0...,0...])", "methodShortName": "SetArraySlice4DFixedDouble6(...)", "fileIndex": 0, "line": 5965,
    "metrics": [
      { "value": 13, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 182, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics/AbsDynamicImplTable`1::.cctor()", "methodShortName": ".cctor()", "fileIndex": 0, "line": 6144,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics/SignDynamicImplTable`1::.cctor()", "methodShortName": ".cctor()", "fileIndex": 0, "line": 6240,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "Microsoft.FSharp.Core.PrintfImpl/Step Microsoft.FSharp.Core.PrintfImpl::buildStep$cont@1134(Microsoft.FSharp.Core.PrintfImpl/FormatSpecifier,System.Type[],System.String,Microsoft.FSharp.Core.Unit)", "methodShortName": "buildStep$cont@1134(...)", "fileIndex": 0, "line": 1134,
    "metrics": [
      { "value": 11, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 59.4, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "System.Tuple`2<Microsoft.FSharp.Linq.QueryModule/TransInnerResult,Microsoft.FSharp.Linq.RuntimeHelpers.Adapters/ConversionDescription> Microsoft.FSharp.Linq.QueryModule::TransFor@1294(Microsoft.FSharp.Linq.QueryModule/CanEliminate,System.Boolean,System.Type,Microsoft.FSharp.Quotations.FSharpVar,System.Type,Microsoft.FSharp.Linq.RuntimeHelpers.Adapters/ConversionDescription,Microsoft.FSharp.Linq.QueryModule/TransInnerResult,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "TransFor@1294(...)", "fileIndex": 0, "line": 1301,
    "metrics": [
      { "value": 14, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 210, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "System.Tuple`2<Microsoft.FSharp.Linq.QueryModule/TransInnerResult,Microsoft.FSharp.Linq.RuntimeHelpers.Adapters/ConversionDescription> Microsoft.FSharp.Linq.QueryModule::TransInner$cont@1274-4(System.Boolean,Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Core.Unit)", "methodShortName": "TransInner$cont@1274-4(...)", "fileIndex": 0, "line": 1579,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.Array", "reportPath": "FSharp.Core_Array.html", "methodName": "T[][] Microsoft.FSharp.Primitives.Basics.Array::splitInto$cont@1166(System.Int32,T[],System.Int32,Microsoft.FSharp.Core.Unit)", "methodShortName": "splitInto$cont@1166(...)", "fileIndex": 0, "line": 1166,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 256, "exceeded": true },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "TState Microsoft.FSharp.Collections.ListModule::foldBack2$cont@327(Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,Microsoft.FSharp.Core.FSharpFunc`2<TState,TState>>>,Microsoft.FSharp.Collections.FSharpList`1<T1>,Microsoft.FSharp.Collections.FSharpList`1<T2>,TState,Microsoft.FSharp.Collections.FSharpList`1<T2>,Microsoft.FSharp.Collections.FSharpList`1<T1>,T2,T1,Microsoft.FSharp.Core.Unit)", "methodShortName": "foldBack2$cont@327(...)", "fileIndex": 0, "line": 327,
    "metrics": [
      { "value": 15, "exceeded": false },
      { "value": 22, "exceeded": false },
      { "value": 240, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "System.Boolean Microsoft.FSharp.Core.PrintfImpl/GenericNumber::isPositive(System.Object)", "methodShortName": "isPositive(...)", "fileIndex": 0, "line": 657,
    "metrics": [
      { "value": 14, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 210, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "System.Reflection.MethodInfo Microsoft.FSharp.Quotations.PatternsModule::bindModuleFunctionWithCallSiteArgs$cont@1110(System.Type,System.String,System.Type[],System.Type[],Microsoft.FSharp.Core.Unit)", "methodShortName": "bindModuleFunctionWithCallSiteArgs$cont@1110(...)", "fileIndex": 0, "line": 1110,
    "metrics": [
      { "value": 14, "exceeded": false },
      { "value": 128, "exceeded": false },
      { "value": 210, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule/InsertManyAt@1547::Close()", "methodShortName": "Close()", "fileIndex": 1, "line": 1547,
    "metrics": [
      { "value": 13, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 182, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.RuntimeHelpers.Adapters", "reportPath": "FSharp.Core_Adapters.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Collections.FSharpList`1<a>> Microsoft.FSharp.Linq.RuntimeHelpers.Adapters::propSetList@69(Microsoft.FSharp.Quotations.FSharpVar,Microsoft.FSharp.Collections.FSharpList`1<a>,Microsoft.FSharp.Collections.FSharpList`1<a>)", "methodShortName": "propSetList@69(...)", "fileIndex": 0, "line": 72,
    "metrics": [
      { "value": 13, "exceeded": false },
      { "value": 192, "exceeded": false },
      { "value": 182, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Int32 Microsoft.FSharp.Reflection.Impl/writeTupleIntoArray@192-2::GenerateNext(System.Collections.Generic.IEnumerable`1<System.Linq.Expressions.Expression>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 0, "line": 193,
    "metrics": [
      { "value": 13, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 182, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule/InsertAt@1533::GenerateNext(System.Collections.Generic.IEnumerable`1<T>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 1, "line": 1533,
    "metrics": [
      { "value": 12, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 156, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.StringModule", "reportPath": "FSharp.Core_StringModule.html", "methodName": "System.String Microsoft.FSharp.Core.StringModule::Replicate(System.Int32,System.String)", "methodShortName": "Replicate(...)", "fileIndex": 0, "line": 133,
    "metrics": [
      { "value": 12, "exceeded": false },
      { "value": 192, "exceeded": false },
      { "value": 156, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.ArrayModule::CompareWith(Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.FSharpFunc`2<T,System.Int32>>,T[],T[])", "methodShortName": "CompareWith(...)", "fileIndex": 0, "line": 1174,
    "metrics": [
      { "value": 11, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 132, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule/Windowed@952::GenerateNext(System.Collections.Generic.IEnumerable`1<T[]>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 1, "line": 952,
    "metrics": [
      { "value": 11, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 132, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule/Skip@1316::GenerateNext(System.Collections.Generic.IEnumerable`1<T>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 1, "line": 1316,
    "metrics": [
      { "value": 11, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 132, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule/InsertAt@1533::Close()", "methodShortName": "Close()", "fileIndex": 1, "line": 1533,
    "metrics": [
      { "value": 11, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 132, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Int32 Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::GenericCompare$cont@886(Microsoft.FSharp.Core.LanguagePrimitives/HashCompare/GenericComparer,System.Object,System.Object,System.Array,Microsoft.FSharp.Core.Unit)", "methodShortName": "GenericCompare$cont@886(...)", "fileIndex": 0, "line": 889,
    "metrics": [
      { "value": 11, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 132, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "System.String Microsoft.FSharp.Core.PrintfImpl/Integer::toString(System.Object)", "methodShortName": "toString(...)", "fileIndex": 0, "line": 742,
    "metrics": [
      { "value": 11, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 132, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "System.String Microsoft.FSharp.Core.PrintfImpl/Integer::toFormattedString(System.String,System.Object)", "methodShortName": "toFormattedString(...)", "fileIndex": 0, "line": 755,
    "metrics": [
      { "value": 11, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 132, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.ExprShapeModule", "reportPath": "FSharp.Core_ExprShapeModule.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Quotations.ExprShapeModule::e$cont@2257(Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Quotations.ExprConstInfo,Microsoft.FSharp.Core.Unit)", "methodShortName": "e$cont@2257(...)", "fileIndex": 0, "line": 2294,
    "metrics": [
      { "value": 11, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 132, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`3<Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr>,System.Reflection.MethodInfo,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>> Microsoft.FSharp.Quotations.PatternsModule::CallPattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "CallPattern(...)", "fileIndex": 0, "line": 556,
    "metrics": [
      { "value": 11, "exceeded": false },
      { "value": 48, "exceeded": false },
      { "value": 132, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule/Take@686::GenerateNext(System.Collections.Generic.IEnumerable`1<T>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 1, "line": 686,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule/Take@686::Close()", "methodShortName": "Close()", "fileIndex": 1, "line": 686,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule/Scan@884::Close()", "methodShortName": "Close()", "fileIndex": 1, "line": 884,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule/Windowed@952::Close()", "methodShortName": "Close()", "fileIndex": 1, "line": 952,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule/Skip@1316::Close()", "methodShortName": "Close()", "fileIndex": 1, "line": 1316,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule/Except@1455::GenerateNext(System.Collections.Generic.IEnumerable`1<T>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 1, "line": 1455,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule/Except@1455::Close()", "methodShortName": "Close()", "fileIndex": 1, "line": 1455,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule/RemoveManyAt@1508::GenerateNext(System.Collections.Generic.IEnumerable`1<T>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 1, "line": 1508,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule/UpdateAt@1520::GenerateNext(System.Collections.Generic.IEnumerable`1<T>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 1, "line": 1520,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule/UpdateAt@1520::Close()", "methodShortName": "Close()", "fileIndex": 1, "line": 1520,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.ExtraTopLevelOperators", "reportPath": "FSharp.Core_ExtraTopLevelOperators.html", "methodName": "T[0...,0...] Microsoft.FSharp.Core.ExtraTopLevelOperators::array2D$cont@192(?[],System.Int32,Microsoft.FSharp.Core.Unit)", "methodShortName": "array2D$cont@192(...)", "fileIndex": 0, "line": 192,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 128, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "TResult Microsoft.FSharp.Core.PrintfImpl/PrintfEnv`3::RunSteps(System.Object[],System.Type[],Microsoft.FSharp.Core.PrintfImpl/Step[])", "methodShortName": "RunSteps(...)", "fileIndex": 0, "line": 320,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 18, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr> Microsoft.FSharp.Linq.QueryModule::|MacroReduction|_|$cont@988(Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Core.Unit)", "methodShortName": "|MacroReduction|_|$cont@988(...)", "fileIndex": 0, "line": 998,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter", "reportPath": "FSharp.Core_LeafExpressionConverter.html", "methodName": "System.Linq.Expressions.Expression Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter::build@610-1(System.Type,System.Linq.Expressions.Expression[])", "methodShortName": "build@610-1(...)", "fileIndex": 0, "line": 611,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.Array", "reportPath": "FSharp.Core_Array.html", "methodName": "System.Void Microsoft.FSharp.Primitives.Basics.Array::stableSortWithKeysAndComparer(System.Collections.Generic.IComparer`1<TKey>,System.Collections.Generic.IComparer`1<TKey>,T[],TKey[])", "methodShortName": "stableSortWithKeysAndComparer(...)", "fileIndex": 0, "line": 1092,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 128, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.FSharpVar", "reportPath": "FSharp.Core_FSharpVar.html", "methodName": "System.Int32 Microsoft.FSharp.Quotations.FSharpVar::System.IComparable.CompareTo(System.Object)", "methodShortName": "System.IComparable.CompareTo(...)", "fileIndex": 0, "line": 123,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`3<System.Type,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpVar>,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Quotations.PatternsModule::NewDelegatePattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "NewDelegatePattern(...)", "fileIndex": 0, "line": 618,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Quotations.PatternsModule/BindingEnv,Microsoft.FSharp.Quotations.FSharpExpr> Microsoft.FSharp.Quotations.PatternsModule::u_Expr(Microsoft.FSharp.Quotations.PatternsModule/SimpleUnpickle/InputState)", "methodShortName": "u_Expr(...)", "fileIndex": 0, "line": 1486,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 9, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Quotations.PatternsModule/ModuleDefinitionBindingResult`2<Microsoft.FSharp.Quotations.ExprConstInfo,Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Quotations.ExprConstInfo>>> Microsoft.FSharp.Quotations.PatternsModule::u_ModuleDefn$cont@1552(Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`2<System.String,System.Int32>>,System.Type,System.String,Microsoft.FSharp.Core.Unit)", "methodShortName": "u_ModuleDefn$cont@1552(...)", "fileIndex": 0, "line": 1552,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Reflection.PropertyInfo[] Microsoft.FSharp.Reflection.Impl::fieldsPropsOfUnionCase(System.Type,System.Int32,System.Reflection.BindingFlags)", "methodShortName": "fieldsPropsOfUnionCase(...)", "fileIndex": 0, "line": 416,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 25, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Type Microsoft.FSharp.Reflection.Impl::makeIt@571(System.Boolean,System.Reflection.Assembly,System.Int32)", "methodShortName": "makeIt@571(...)", "fileIndex": 0, "line": 578,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Void Microsoft.FSharp.Reflection.Impl/writeTupleIntoArray@192-2::Close()", "methodShortName": "Close()", "fileIndex": 0, "line": 192,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "reportPath": "FSharp.Core_Display.html", "methodName": "Microsoft.FSharp.Text.StructuredPrintfImpl.Layout Microsoft.FSharp.Text.StructuredPrintfImpl.Display/ObjectGraphFormatter::objectValueWithPropertiesL(System.Int32,System.Type,System.Object)", "methodShortName": "objectValueWithPropertiesL(...)", "fileIndex": 0, "line": 1180,
    "metrics": [
      { "value": 10, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 110, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[][] Microsoft.FSharp.Collections.ArrayModule::transposeArrays(T[][])", "methodShortName": "transposeArrays(...)", "fileIndex": 0, "line": 1242,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 128, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.KeyCollection<T1, T2>", "reportPath": "FSharp.Core_KeyCollection_2.html", "methodName": "System.Void <StartupCode$FSharp-Core>.$Map/System-Collections-Generic-IEnumerable<'Key>-GetEnumerator@851::Close()", "methodShortName": "Close()", "fileIndex": 0, "line": 851,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.KeyCollection<T1, T2>", "reportPath": "FSharp.Core_KeyCollection_2.html", "methodName": "System.Void <StartupCode$FSharp-Core>.$Map/System-Collections-IEnumerable-GetEnumerator@855::Close()", "methodShortName": "Close()", "fileIndex": 0, "line": 855,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.MapTreeModule", "reportPath": "FSharp.Core_MapTreeModule.html", "methodName": "Microsoft.FSharp.Collections.MapTree`2<TKey,TValue> Microsoft.FSharp.Collections.MapTreeModule::rebalance(Microsoft.FSharp.Collections.MapTree`2<TKey,TValue>,TKey,TValue,Microsoft.FSharp.Collections.MapTree`2<TKey,TValue>)", "methodShortName": "rebalance(...)", "fileIndex": 0, "line": 108,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule/Truncate@863::Close()", "methodShortName": "Close()", "fileIndex": 1, "line": 863,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule/Pairwise@872::Close()", "methodShortName": "Close()", "fileIndex": 1, "line": 872,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule/Distinct@1090::Close()", "methodShortName": "Close()", "fileIndex": 1, "line": 1090,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule/DistinctBy@1098::Close()", "methodShortName": "Close()", "fileIndex": 1, "line": 1098,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule/TakeWhile@1308::Close()", "methodShortName": "Close()", "fileIndex": 1, "line": 1308,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule/SkipWhile@1327::Close()", "methodShortName": "Close()", "fileIndex": 1, "line": 1327,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule/Tail@1376::Close()", "methodShortName": "Close()", "fileIndex": 1, "line": 1376,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule/ChunkBySize@1469::Close()", "methodShortName": "Close()", "fileIndex": 1, "line": 1469,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule/RemoveAt@1496::GenerateNext(System.Collections.Generic.IEnumerable`1<T>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 1, "line": 1496,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule/RemoveAt@1496::Close()", "methodShortName": "Close()", "fileIndex": 1, "line": 1496,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule/RemoveManyAt@1508::Close()", "methodShortName": "Close()", "fileIndex": 1, "line": 1508,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SetTreeModule", "reportPath": "FSharp.Core_SetTreeModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SetTreeModule::cont@464(System.Collections.Generic.IComparer`1<T>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Collections.SetTree`1<T>>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Collections.SetTree`1<T>>,Microsoft.FSharp.Core.Unit)", "methodShortName": "cont@464(...)", "fileIndex": 0, "line": 466,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ValueCollection<T1, T2>", "reportPath": "FSharp.Core_ValueCollection_2.html", "methodName": "System.Void <StartupCode$FSharp-Core>.$Map/System-Collections-Generic-IEnumerable<'Value>-GetEnumerator@883::Close()", "methodShortName": "Close()", "fileIndex": 0, "line": 883,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ValueCollection<T1, T2>", "reportPath": "FSharp.Core_ValueCollection_2.html", "methodName": "System.Void <StartupCode$FSharp-Core>.$Map/System-Collections-IEnumerable-GetEnumerator@887-1::Close()", "methodShortName": "Close()", "fileIndex": 0, "line": 887,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Collections.Generic.IEnumerator`1<System.Double> Microsoft.FSharp.Core.Operators/OperatorIntrinsics::gen@5470(System.Double,System.Double,System.Double,Microsoft.FSharp.Core.Unit)", "methodShortName": "gen@5470(...)", "fileIndex": 0, "line": 5486,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 6, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Collections.Generic.IEnumerator`1<System.Single> Microsoft.FSharp.Core.Operators/OperatorIntrinsics::gen@5470-1(System.Single,System.Single,System.Single,Microsoft.FSharp.Core.Unit)", "methodShortName": "gen@5470-1(...)", "fileIndex": 0, "line": 5487,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 6, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "System.Object Microsoft.FSharp.Linq.QueryModule::Call$cont@628(Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<System.Object>>,System.Object>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<System.Object>>,System.Object>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<System.Object>>,System.Object>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<System.Object>>,System.Object>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<System.Object>>,System.Object>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`3<System.Object,Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<System.Object>>,System.Object>,Microsoft.FSharp.Quotations.FSharpVar,System.Type,System.Object,System.Type,System.Type,Microsoft.FSharp.Quotations.FSharpExpr,System.Object,Microsoft.FSharp.Core.Unit)", "methodShortName": "Call$cont@628(...)", "fileIndex": 0, "line": 630,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr> Microsoft.FSharp.Linq.QueryModule::|MacroReduction|_|$cont@988-1(Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Core.Unit)", "methodShortName": "|MacroReduction|_|$cont@988-1(...)", "fileIndex": 0, "line": 1023,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 128, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Core.Unit> Microsoft.FSharp.Linq.QueryModule::|ZeroOnElseBranch|_|(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "|ZeroOnElseBranch|_|(...)", "fileIndex": 0, "line": 1145,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Linq.QueryModule::ConvMutableToImmutable(Microsoft.FSharp.Linq.RuntimeHelpers.Adapters/ConversionDescription,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "ConvMutableToImmutable(...)", "fileIndex": 0, "line": 1153,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "System.Tuple`2<Microsoft.FSharp.Linq.QueryModule/TransInnerResult,Microsoft.FSharp.Linq.RuntimeHelpers.Adapters/ConversionDescription> Microsoft.FSharp.Linq.QueryModule::TransInner$cont@1274-3(System.Boolean,Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Core.Unit)", "methodShortName": "TransInner$cont@1274-3(...)", "fileIndex": 0, "line": 1567,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 128, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.RuntimeHelpers.Adapters", "reportPath": "FSharp.Core_Adapters.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Linq.RuntimeHelpers.Adapters::NewAnonymousObject(Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>)", "methodShortName": "NewAnonymousObject(...)", "fileIndex": 0, "line": 136,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<T> Microsoft.FSharp.Primitives.Basics.List::concat(System.Collections.Generic.IEnumerable`1<Microsoft.FSharp.Collections.FSharpList`1<T>>)", "methodShortName": "concat(...)", "fileIndex": 0, "line": 550,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 48, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.Seq", "reportPath": "FSharp.Core_Seq.html", "methodName": "Microsoft.FSharp.Core.FSharpValueOption`1<T> Microsoft.FSharp.Primitives.Basics.Seq::tryLastV(System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "tryLastV(...)", "fileIndex": 0, "line": 1183,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`5<Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpVar,Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpVar,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Quotations.PatternsModule::TryWithPattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "TryWithPattern(...)", "fileIndex": 0, "line": 472,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`3<Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr>,System.Reflection.FieldInfo,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Quotations.PatternsModule::FieldSetPattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "FieldSetPattern(...)", "fileIndex": 0, "line": 544,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 6, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Type Microsoft.FSharp.Reflection.Impl::getUnionCaseTyp(System.Type,System.Int32,System.Reflection.BindingFlags)", "methodShortName": "getUnionCaseTyp(...)", "fileIndex": 0, "line": 360,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 192, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "reportPath": "FSharp.Core_Display.html", "methodName": "System.Tuple`4<Microsoft.FSharp.Text.StructuredPrintfImpl.Display/Breaks,Microsoft.FSharp.Text.StructuredPrintfImpl.Layout,System.Int32,System.Int32> Microsoft.FSharp.Text.StructuredPrintfImpl.Display::fit@598(System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Object,Microsoft.FSharp.Text.StructuredPrintfImpl.TaggedText>,Microsoft.FSharp.Text.StructuredPrintfImpl.Display/Breaks,System.Int32,Microsoft.FSharp.Text.StructuredPrintfImpl.Layout)", "methodShortName": "fit@598(...)", "fileIndex": 0, "line": 670,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 128, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "ImageProcessing", "class": "ImageProcessing.ImageProcessing", "reportPath": "ImageProcessing_ImageProcessing.html", "methodName": "System.Single ImageProcessing.ImageProcessing/processPixel@85::Invoke(System.Int32,System.Int32)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 85,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 90, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "Microsoft.FSharp.Core.PrintfImpl/ValueConverter Microsoft.FSharp.Core.PrintfImpl::getValueConverter(System.Type,Microsoft.FSharp.Core.PrintfImpl/FormatSpecifier)", "methodShortName": "getValueConverter(...)", "fileIndex": 0, "line": 982,
    "metrics": [
      { "value": 11, "exceeded": false },
      { "value": 27, "exceeded": false },
      { "value": 81.01, "exceeded": true },
    ]},
  {
    "assembly": "AltCover.Monitor", "class": "AltCover.Monitor", "reportPath": "AltCover.Monitor_Monitor.html", "methodName": "System.Boolean AltCover.Monitor::TryGetPointTotals(AltCover.PointCount&)", "methodShortName": "TryGetPointTotals(...)", "fileIndex": 0, "line": 165,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "AltCover.Monitor", "class": "AltCover.Monitor", "reportPath": "AltCover.Monitor_Monitor.html", "methodName": "TypeInstance()", "methodShortName": "TypeInstance()", "fileIndex": 0, "line": 124,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Array3DModule", "reportPath": "FSharp.Core_Array3DModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.Array3DModule::Iterate(Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.Unit>,T[0...,0...,0...])", "methodShortName": "Iterate(...)", "fileIndex": 0, "line": 64,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Array3DModule", "reportPath": "FSharp.Core_Array3DModule.html", "methodName": "TResult[0...,0...,0...] Microsoft.FSharp.Collections.Array3DModule::Map(Microsoft.FSharp.Core.FSharpFunc`2<T,TResult>,T[0...,0...,0...])", "methodShortName": "Map(...)", "fileIndex": 0, "line": 75,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Array3DModule", "reportPath": "FSharp.Core_Array3DModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.Array3DModule::IterateIndexed(Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.Unit>>>>,T[0...,0...,0...])", "methodShortName": "IterateIndexed(...)", "fileIndex": 0, "line": 88,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Array3DModule", "reportPath": "FSharp.Core_Array3DModule.html", "methodName": "TResult[0...,0...,0...] Microsoft.FSharp.Collections.Array3DModule::MapIndexed(Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<T,TResult>>>>,T[0...,0...,0...])", "methodShortName": "MapIndexed(...)", "fileIndex": 0, "line": 100,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Tuple`2<T[],T[]> Microsoft.FSharp.Collections.ArrayModule::splitAt$cont@130(System.Int32,T[],Microsoft.FSharp.Core.Unit)", "methodShortName": "splitAt$cont@130(...)", "fileIndex": 0, "line": 130,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 128, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[] Microsoft.FSharp.Collections.ArrayModule::SkipWhile(Microsoft.FSharp.Core.FSharpFunc`2<T,System.Boolean>,T[])", "methodShortName": "SkipWhile(...)", "fileIndex": 0, "line": 748,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[][] Microsoft.FSharp.Collections.ArrayModule::Windowed(System.Int32,T[])", "methodShortName": "Windowed(...)", "fileIndex": 0, "line": 778,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[][] Microsoft.FSharp.Collections.ArrayModule::chunkBySize$cont@798(System.Int32,T[],System.Int32,Microsoft.FSharp.Core.Unit)", "methodShortName": "chunkBySize$cont@798(...)", "fileIndex": 0, "line": 798,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 128, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.MapTreeModule", "reportPath": "FSharp.Core_MapTreeModule.html", "methodName": "Microsoft.FSharp.Collections.MapTree`2<TKey,TValue> Microsoft.FSharp.Collections.MapTreeModule::remove(System.Collections.Generic.IComparer`1<TKey>,TKey,Microsoft.FSharp.Collections.MapTree`2<TKey,TValue>)", "methodShortName": "remove(...)", "fileIndex": 0, "line": 219,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.SeqModule::ForAll2(Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,System.Boolean>>,System.Collections.Generic.IEnumerable`1<T1>,System.Collections.Generic.IEnumerable`1<T2>)", "methodShortName": "ForAll2(...)", "fileIndex": 1, "line": 1339,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.SeqModule::Exists2(Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,System.Boolean>>,System.Collections.Generic.IEnumerable`1<T1>,System.Collections.Generic.IEnumerable`1<T2>)", "methodShortName": "Exists2(...)", "fileIndex": 1, "line": 1351,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule/SkipWhile@1327::GenerateNext(System.Collections.Generic.IEnumerable`1<T>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 1, "line": 1327,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule/Tail@1376::GenerateNext(System.Collections.Generic.IEnumerable`1<T>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 1, "line": 1376,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SetTreeModule", "reportPath": "FSharp.Core_SetTreeModule.html", "methodName": "Microsoft.FSharp.Collections.SetTree`1<T> Microsoft.FSharp.Collections.SetTreeModule::remove(System.Collections.Generic.IComparer`1<T>,T,Microsoft.FSharp.Collections.SetTree`1<T>)", "methodShortName": "remove(...)", "fileIndex": 0, "line": 205,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.FSharpEvent<T1, T2>", "reportPath": "FSharp.Core_FSharpEvent_2.html", "methodName": "System.Void Microsoft.FSharp.Control.FSharpEvent`2::.cctor()", "methodShortName": ".cctor()", "fileIndex": 0, "line": 76,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.CompilerServices.ArrayCollector<T>", "reportPath": "FSharp.Core_ArrayCollector_1.html", "methodName": "System.Void Microsoft.FSharp.Core.CompilerServices.ArrayCollector`1::AddMany(System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "AddMany(...)", "fileIndex": 0, "line": 503,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Collections.Generic.IEqualityComparer`1<T> Microsoft.FSharp.Core.LanguagePrimitives::.cctor$cont@2085(System.Type,Microsoft.FSharp.Core.Unit)", "methodShortName": ".cctor$cont@2085(...)", "fileIndex": 0, "line": 2088,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 128, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Collections.Generic.IEqualityComparer`1<T> Microsoft.FSharp.Core.LanguagePrimitives::.cctor$cont@2085-1(System.Type,Microsoft.FSharp.Core.Unit)", "methodShortName": ".cctor$cont@2085-1(...)", "fileIndex": 0, "line": 2095,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 128, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.UInt32 Microsoft.FSharp.Core.LanguagePrimitives::ParseUInt32(System.String)", "methodShortName": "ParseUInt32(...)", "fileIndex": 0, "line": 2360,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 24, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Int32 Microsoft.FSharp.Core.LanguagePrimitives::ParseInt32(System.String)", "methodShortName": "ParseInt32(...)", "fileIndex": 0, "line": 2377,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 24, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Int64 Microsoft.FSharp.Core.LanguagePrimitives::ParseInt64(System.String)", "methodShortName": "ParseInt64(...)", "fileIndex": 0, "line": 2392,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 24, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.UInt64 Microsoft.FSharp.Core.LanguagePrimitives::ParseUInt64(System.String)", "methodShortName": "ParseUInt64(...)", "fileIndex": 0, "line": 2407,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 24, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Int32 Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::GenericHashParamObj(System.Collections.IEqualityComparer,System.Object)", "methodShortName": "GenericHashParamObj(...)", "fileIndex": 0, "line": 1695,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Int32 Microsoft.FSharp.Core.LanguagePrimitives/IntrinsicFunctions::getTypeInfo(System.Type)", "methodShortName": "getTypeInfo(...)", "fileIndex": 0, "line": 604,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 128, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice(T[],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "GetArraySlice(...)", "fileIndex": 0, "line": 5567,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice2DFixed1(T[0...,0...],System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "GetArraySlice2DFixed1(...)", "fileIndex": 0, "line": 5598,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice2DFixed2(T[0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32)", "methodShortName": "GetArraySlice2DFixed2(...)", "fileIndex": 0, "line": 5600,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice3DFixedDouble1(T[0...,0...,0...],System.Int32,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "GetArraySlice3DFixedDouble1(...)", "fileIndex": 0, "line": 5683,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice3DFixedDouble2(T[0...,0...,0...],System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32)", "methodShortName": "GetArraySlice3DFixedDouble2(...)", "fileIndex": 0, "line": 5686,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice3DFixedDouble3(T[0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,System.Int32)", "methodShortName": "GetArraySlice3DFixedDouble3(...)", "fileIndex": 0, "line": 5689,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice4DFixedTriple1(T[0...,0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,System.Int32,System.Int32)", "methodShortName": "GetArraySlice4DFixedTriple1(...)", "fileIndex": 0, "line": 5865,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice4DFixedTriple2(T[0...,0...,0...,0...],System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,System.Int32)", "methodShortName": "GetArraySlice4DFixedTriple2(...)", "fileIndex": 0, "line": 5868,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice4DFixedTriple3(T[0...,0...,0...,0...],System.Int32,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32)", "methodShortName": "GetArraySlice4DFixedTriple3(...)", "fileIndex": 0, "line": 5871,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T[] Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetArraySlice4DFixedTriple4(T[0...,0...,0...,0...],System.Int32,System.Int32,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "GetArraySlice4DFixedTriple4(...)", "fileIndex": 0, "line": 5874,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "Microsoft.FSharp.Core.PrintfImpl/ValueConverter Microsoft.FSharp.Core.PrintfImpl/ObjectPrinter::GenericToString(Microsoft.FSharp.Core.PrintfImpl/FormatSpecifier)", "methodShortName": "GenericToString(...)", "fileIndex": 0, "line": 934,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 96, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "System.Boolean Microsoft.FSharp.Core.PrintfImpl/FloatAndDecimal::isNumber(System.Object)", "methodShortName": "isNumber(...)", "fileIndex": 0, "line": 858,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "Microsoft.FSharp.Core.PrintfImpl/ValueConverter Microsoft.FSharp.Core.PrintfImpl/Integer::getValueConverter(Microsoft.FSharp.Core.PrintfImpl/FormatSpecifier)", "methodShortName": "getValueConverter(...)", "fileIndex": 0, "line": 824,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 14, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "Microsoft.FSharp.Core.PrintfImpl/ValueConverter Microsoft.FSharp.Core.PrintfImpl/Padding::adaptPaddedFormatted(Microsoft.FSharp.Core.PrintfImpl/FormatSpecifier,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,System.String>,Microsoft.FSharp.Core.FSharpFunc`2<System.String,Microsoft.FSharp.Core.FSharpFunc`2<System.Object,System.String>>,Microsoft.FSharp.Core.FSharpFunc`2<System.String,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Object,System.String>>>)", "methodShortName": "adaptPaddedFormatted(...)", "fileIndex": 0, "line": 566,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 128, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "System.Tuple`2<Microsoft.FSharp.Linq.QueryModule/TransInnerResult,Microsoft.FSharp.Linq.RuntimeHelpers.Adapters/ConversionDescription> Microsoft.FSharp.Linq.QueryModule::TransInner$cont@1274-2(System.Boolean,Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Core.Unit)", "methodShortName": "TransInner$cont@1274-2(...)", "fileIndex": 0, "line": 1556,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter", "reportPath": "FSharp.Core_LeafExpressionConverter.html", "methodName": "System.Linq.Expressions.Expression Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter::transBinOp(Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter/ConvEnv,System.Boolean,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>,System.Boolean,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<System.Linq.Expressions.Expression,System.Linq.Expressions.Expression>,System.Linq.Expressions.BinaryExpression>)", "methodShortName": "transBinOp(...)", "fileIndex": 0, "line": 671,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`4<Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr>,System.Reflection.PropertyInfo,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Quotations.PatternsModule::PropertySetPattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "PropertySetPattern(...)", "fileIndex": 0, "line": 529,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "System.Reflection.MethodInfo Microsoft.FSharp.Quotations.PatternsModule::bindMethodHelper(System.Type,System.String,System.Int32,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,System.Type>,System.Type>>,Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,System.Type>,System.Type>)", "methodShortName": "bindMethodHelper(...)", "fileIndex": 0, "line": 1075,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 128, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Quotations.PatternsModule::substituteInExpr(Microsoft.FSharp.Collections.FSharpSet`1<Microsoft.FSharp.Quotations.FSharpVar>,Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Quotations.FSharpVar,Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr>>,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "substituteInExpr(...)", "fileIndex": 0, "line": 1760,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`3<Microsoft.FSharp.Core.SourceConstructFlags,System.Int32,System.Int32>> Microsoft.FSharp.Reflection.Impl::tryFindCompilationMappingAttributeFromData$cont@264(System.Collections.Generic.IList`1<System.Reflection.CustomAttributeData>,Microsoft.FSharp.Core.Unit)", "methodShortName": "tryFindCompilationMappingAttributeFromData$cont@264(...)", "fileIndex": 0, "line": 264,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`2<System.Int32,System.String>> Microsoft.FSharp.Reflection.Impl::getUnionTypeTagNameMap$cont@342(System.Type,System.Reflection.MethodInfo,System.Int32,Microsoft.FSharp.Core.Unit)", "methodShortName": "getUnionTypeTagNameMap$cont@342(...)", "fileIndex": 0, "line": 342,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 96, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Object Microsoft.FSharp.Reflection.Impl/getTupleConstructor@720::Invoke(System.Object[])", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 721,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "reportPath": "FSharp.Core_Display.html", "methodName": "System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.String>,System.Int32> Microsoft.FSharp.Text.StructuredPrintfImpl.Display::addL@692(Microsoft.FSharp.Text.StructuredPrintfImpl.FormatOptions,Microsoft.FSharp.Core.FSharpFunc`2<System.Object,System.String>,System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.String>,System.Int32>,System.Int32,Microsoft.FSharp.Text.StructuredPrintfImpl.Layout)", "methodShortName": "addL@692(...)", "fileIndex": 0, "line": 696,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 24, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "reportPath": "FSharp.Core_Display.html", "methodName": "System.String Microsoft.FSharp.Text.StructuredPrintfImpl.Display::formatChar(System.Boolean,System.Char)", "methodShortName": "formatChar(...)", "fileIndex": 0, "line": 825,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "ImageProcessing", "class": "ImageProcessing.ImageProcessing", "reportPath": "ImageProcessing_ImageProcessing.html", "methodName": "System.Int32 ImageProcessing.ImageProcessing/Pipe #1 input at line 46@47::GenerateNext(System.Collections.Generic.IEnumerable`1<a>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 0, "line": 47,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 72, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Array3DModule", "reportPath": "FSharp.Core_Array3DModule.html", "methodName": "T[0...,0...,0...] Microsoft.FSharp.Collections.Array3DModule::Create(System.Int32,System.Int32,System.Int32,T)", "methodShortName": "Create(...)", "fileIndex": 0, "line": 44,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Array3DModule", "reportPath": "FSharp.Core_Array3DModule.html", "methodName": "T[0...,0...,0...] Microsoft.FSharp.Collections.Array3DModule::Initialize(System.Int32,System.Int32,System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,T>>>)", "methodShortName": "Initialize(...)", "fileIndex": 0, "line": 53,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Tuple`2<T[],T[]> Microsoft.FSharp.Collections.ArrayModule::SplitAt(System.Int32,T[])", "methodShortName": "SplitAt(...)", "fileIndex": 0, "line": 125,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[] Microsoft.FSharp.Collections.ArrayModule::Take(System.Int32,T[])", "methodShortName": "Take(...)", "fileIndex": 0, "line": 142,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[] Microsoft.FSharp.Collections.ArrayModule::TakeWhile(Microsoft.FSharp.Core.FSharpFunc`2<T,System.Boolean>,T[])", "methodShortName": "TakeWhile(...)", "fileIndex": 0, "line": 154,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[] Microsoft.FSharp.Collections.ArrayModule::DistinctBy(Microsoft.FSharp.Core.FSharpFunc`2<T,TKey>,T[])", "methodShortName": "DistinctBy(...)", "fileIndex": 0, "line": 280,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "TResult[] Microsoft.FSharp.Collections.ArrayModule::Map3(Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,Microsoft.FSharp.Core.FSharpFunc`2<T3,TResult>>>,T1[],T2[],T3[])", "methodShortName": "Map3(...)", "fileIndex": 0, "line": 309,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Tuple`2<T[],T[]> Microsoft.FSharp.Collections.ArrayModule::Partition(Microsoft.FSharp.Core.FSharpFunc`2<T,System.Boolean>,T[])", "methodShortName": "Partition(...)", "fileIndex": 0, "line": 698,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[] Microsoft.FSharp.Collections.ArrayModule::Skip(System.Int32,T[])", "methodShortName": "Skip(...)", "fileIndex": 0, "line": 738,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Tuple`3<T1,T2,T3>[] Microsoft.FSharp.Collections.ArrayModule::Zip3(T1[],T2[],T3[])", "methodShortName": "Zip3(...)", "fileIndex": 0, "line": 828,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[] Microsoft.FSharp.Collections.ArrayModule::GetSubArray(T[],System.Int32,System.Int32)", "methodShortName": "GetSubArray(...)", "fileIndex": 0, "line": 1197,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[] Microsoft.FSharp.Collections.ArrayModule::InsertManyAt(System.Int32,System.Collections.Generic.IEnumerable`1<T>,T[])", "methodShortName": "InsertManyAt(...)", "fileIndex": 0, "line": 1337,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.ArrayModule/Filter::createMask(Microsoft.FSharp.Core.FSharpFunc`2<a,System.Boolean>,a[],System.UInt32[]&,System.UInt32&)", "methodShortName": "createMask(...)", "fileIndex": 0, "line": 566,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.MapTreeModule", "reportPath": "FSharp.Core_MapTreeModule.html", "methodName": "Microsoft.FSharp.Collections.MapTree`2<TKey,TValue> Microsoft.FSharp.Collections.MapTreeModule::add(System.Collections.Generic.IComparer`1<TKey>,TKey,TValue,Microsoft.FSharp.Collections.MapTree`2<TKey,TValue>)", "methodShortName": "add(...)", "fileIndex": 0, "line": 131,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule::Iterate2(Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,Microsoft.FSharp.Core.Unit>>,System.Collections.Generic.IEnumerable`1<T1>,System.Collections.Generic.IEnumerable`1<T2>)", "methodShortName": "Iterate2(...)", "fileIndex": 1, "line": 557,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule::IterateIndexed2(Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,Microsoft.FSharp.Core.Unit>>>,System.Collections.Generic.IEnumerable`1<T1>,System.Collections.Generic.IEnumerable`1<T2>)", "methodShortName": "IterateIndexed2(...)", "fileIndex": 1, "line": 567,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "TState Microsoft.FSharp.Collections.SeqModule::Fold2(Microsoft.FSharp.Core.FSharpFunc`2<TState,Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,TState>>>,TState,System.Collections.Generic.IEnumerable`1<T1>,System.Collections.Generic.IEnumerable`1<T2>)", "methodShortName": "Fold2(...)", "fileIndex": 1, "line": 739,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule::go@784(System.Collections.Generic.IEnumerator`1<T>,System.Collections.Generic.IEnumerator`1<T>,Microsoft.FSharp.Core.OptimizedClosures/FSharpFunc`3<T,T,System.Int32>,Microsoft.FSharp.Core.Unit)", "methodShortName": "go@784(...)", "fileIndex": 1, "line": 785,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule/Truncate@863::GenerateNext(System.Collections.Generic.IEnumerable`1<T>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 1, "line": 863,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule/Pairwise@872::GenerateNext(System.Collections.Generic.IEnumerable`1<System.Tuple`2<T,T>>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 1, "line": 872,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule/Scan@884::GenerateNext(System.Collections.Generic.IEnumerable`1<TState>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 1, "line": 884,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule/Distinct@1090::GenerateNext(System.Collections.Generic.IEnumerable`1<T>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 1, "line": 1090,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule/DistinctBy@1098::GenerateNext(System.Collections.Generic.IEnumerable`1<T>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 1, "line": 1098,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule/TakeWhile@1308::GenerateNext(System.Collections.Generic.IEnumerable`1<T>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 1, "line": 1308,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SetTreeModule", "reportPath": "FSharp.Core_SetTreeModule.html", "methodName": "Microsoft.FSharp.Collections.SetTree`1<T> Microsoft.FSharp.Collections.SetTreeModule::balance(System.Collections.Generic.IComparer`1<T>,Microsoft.FSharp.Collections.SetTree`1<T>,T,Microsoft.FSharp.Collections.SetTree`1<T>)", "methodShortName": "balance(...)", "fileIndex": 0, "line": 148,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SetTreeModule", "reportPath": "FSharp.Core_SetTreeModule.html", "methodName": "System.Tuple`3<Microsoft.FSharp.Collections.SetTree`1<T>,System.Boolean,Microsoft.FSharp.Collections.SetTree`1<T>> Microsoft.FSharp.Collections.SetTreeModule::split(System.Collections.Generic.IComparer`1<T>,T,Microsoft.FSharp.Collections.SetTree`1<T>)", "methodShortName": "split(...)", "fileIndex": 0, "line": 176,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.AsyncPrimitives", "reportPath": "FSharp.Core_AsyncPrimitives.html", "methodName": "Microsoft.FSharp.Control.AsyncReturn Microsoft.FSharp.Control.AsyncPrimitives/SuspendedAsync`1::ContinueImmediate(T)", "methodShortName": "ContinueImmediate(...)", "fileIndex": 0, "line": 841,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.FSharpAsync", "reportPath": "FSharp.Core_FSharpAsync.html", "methodName": "Microsoft.FSharp.Control.AsyncReturn <StartupCode$FSharp-Core>.$Async/Parallel@1401-1::Invoke(Microsoft.FSharp.Core.Unit)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 1402,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.CompilerServices.GeneratedSequenceBase<T>", "reportPath": "FSharp.Core_GeneratedSequenceBase_1.html", "methodName": "System.Boolean Microsoft.FSharp.Core.CompilerServices.GeneratedSequenceBase`1::MoveNextImpl()", "methodShortName": "MoveNextImpl()", "fileIndex": 0, "line": 369,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 24, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.CompilerServices.ListCollector<T>", "reportPath": "FSharp.Core_ListCollector_1.html", "methodName": "System.Void Microsoft.FSharp.Core.CompilerServices.ListCollector`1::AddMany(System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "AddMany(...)", "fileIndex": 0, "line": 431,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Void Microsoft.FSharp.Core.LanguagePrimitives/GenericZeroDynamicImplTable`1::.cctor()", "methodShortName": ".cctor()", "fileIndex": 0, "line": 2438,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Void Microsoft.FSharp.Core.LanguagePrimitives/GenericOneDynamicImplTable`1::.cctor()", "methodShortName": ".cctor()", "fileIndex": 0, "line": 2462,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.NumericLiterals", "reportPath": "FSharp.Core_NumericLiterals.html", "methodName": "System.Object Microsoft.FSharp.Core.NumericLiterals/NumericLiteralI::action@4793-5(System.String,Microsoft.FSharp.Core.Unit)", "methodShortName": "action@4793-5(...)", "fileIndex": 0, "line": 65,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice2DFixed1(T[0...,0...],System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,T[])", "methodShortName": "SetArraySlice2DFixed1(...)", "fileIndex": 0, "line": 5614,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice2DFixed2(T[0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,T[])", "methodShortName": "SetArraySlice2DFixed2(...)", "fileIndex": 0, "line": 5616,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice3DFixedDouble1(T[0...,0...,0...],System.Int32,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,T[])", "methodShortName": "SetArraySlice3DFixedDouble1(...)", "fileIndex": 0, "line": 5744,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice3DFixedDouble2(T[0...,0...,0...],System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,T[])", "methodShortName": "SetArraySlice3DFixedDouble2(...)", "fileIndex": 0, "line": 5747,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice3DFixedDouble3(T[0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,System.Int32,T[])", "methodShortName": "SetArraySlice3DFixedDouble3(...)", "fileIndex": 0, "line": 5750,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice4DFixedTriple1(T[0...,0...,0...,0...],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,System.Int32,System.Int32,T[])", "methodShortName": "SetArraySlice4DFixedTriple1(...)", "fileIndex": 0, "line": 5982,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice4DFixedTriple2(T[0...,0...,0...,0...],System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,System.Int32,T[])", "methodShortName": "SetArraySlice4DFixedTriple2(...)", "fileIndex": 0, "line": 5985,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice4DFixedTriple3(T[0...,0...,0...,0...],System.Int32,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,System.Int32,T[])", "methodShortName": "SetArraySlice4DFixedTriple3(...)", "fileIndex": 0, "line": 5988,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice4DFixedTriple4(T[0...,0...,0...,0...],System.Int32,System.Int32,System.Int32,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,T[])", "methodShortName": "SetArraySlice4DFixedTriple4(...)", "fileIndex": 0, "line": 5991,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "System.Object Microsoft.FSharp.Core.PrintfImpl/Integer::toUnsigned(System.Object)", "methodShortName": "toUnsigned(...)", "fileIndex": 0, "line": 768,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.StringModule", "reportPath": "FSharp.Core_StringModule.html", "methodName": "System.String Microsoft.FSharp.Core.StringModule::Filter(Microsoft.FSharp.Core.FSharpFunc`2<System.Char,System.Boolean>,System.String)", "methodShortName": "Filter(...)", "fileIndex": 0, "line": 90,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "System.Object Microsoft.FSharp.Linq.QueryModule::Call@614-3(Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<System.Object>>,System.Object>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<System.Object>>,System.Object>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<System.Object>>,System.Object>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<System.Object>>,System.Object>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<System.Object>>,System.Object>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<System.Object>>,System.Object>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<System.Object>>,System.Object>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<System.Object>>,System.Object>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<System.Object>>,System.Object>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<System.Object>>,System.Object>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`3<System.Object,Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<System.Object>>,System.Object>,System.Object,System.Boolean,System.Type,System.Type,System.Object,System.Type,Microsoft.FSharp.Quotations.FSharpVar,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "Call@614-3(...)", "fileIndex": 0, "line": 615,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`5<Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Linq.QueryModule/|SpecificCall3|_|@335::Invoke(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 336,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.RuntimeHelpers.Adapters", "reportPath": "FSharp.Core_Adapters.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr> Microsoft.FSharp.Linq.RuntimeHelpers.Adapters::|RecordFieldGetSimplification|_|(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "|RecordFieldGetSimplification|_|(...)", "fileIndex": 0, "line": 169,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.RuntimeHelpers.Adapters", "reportPath": "FSharp.Core_Adapters.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Linq.RuntimeHelpers.Adapters::CleanupLeaf(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "CleanupLeaf(...)", "fileIndex": 0, "line": 228,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 48, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<T>,Microsoft.FSharp.Collections.FSharpList`1<T>> Microsoft.FSharp.Primitives.Basics.List::splitAt(System.Int32,Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "splitAt(...)", "fileIndex": 0, "line": 604,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 64, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "System.Tuple`3<Microsoft.FSharp.Collections.FSharpList`1<a>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Collections.FSharpList`1<a>>,System.Int32> Microsoft.FSharp.Primitives.Basics.List::transposeGetHeads(Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Collections.FSharpList`1<a>>)", "methodShortName": "transposeGetHeads(...)", "fileIndex": 0, "line": 694,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`2<Microsoft.FSharp.Quotations.FSharpVar,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Quotations.PatternsModule::VarSetPattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "VarSetPattern(...)", "fileIndex": 0, "line": 475,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`2<Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr>,System.Reflection.FieldInfo>> Microsoft.FSharp.Quotations.PatternsModule::FieldGetPattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "FieldGetPattern(...)", "fileIndex": 0, "line": 537,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`5<Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr>,System.Reflection.MethodInfo,System.Reflection.MethodInfo,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>> Microsoft.FSharp.Quotations.PatternsModule::CallWithWitnessesPattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "CallWithWitnessesPattern(...)", "fileIndex": 0, "line": 575,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Quotations.PatternsModule::fillHolesInRawExpr(Microsoft.FSharp.Quotations.FSharpExpr[],Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "fillHolesInRawExpr(...)", "fileIndex": 0, "line": 1726,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "System.Byte[] Microsoft.FSharp.Quotations.PatternsModule/ByteStream::ReadBytes(System.Int32)", "methodShortName": "ReadBytes(...)", "fileIndex": 0, "line": 364,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Type[] Microsoft.FSharp.Reflection.Impl::getTupleTypeInfo(System.Type)", "methodShortName": "getTupleTypeInfo(...)", "fileIndex": 0, "line": 617,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Reflection.ConstructorInfo Microsoft.FSharp.Reflection.Impl::getTupleConstructorMethod(System.Type)", "methodShortName": "getTupleConstructorMethod(...)", "fileIndex": 0, "line": 673,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "reportPath": "FSharp.Core_Display.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`2<System.Tuple`2<System.Object,System.Type>,System.Tuple`2<System.Object,System.Type>>> Microsoft.FSharp.Text.StructuredPrintfImpl.Display::getListValueInfo(System.Reflection.BindingFlags,System.Object,System.Type)", "methodShortName": "getListValueInfo(...)", "fileIndex": 0, "line": 772,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 6, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "reportPath": "FSharp.Core_Display.html", "methodName": "Microsoft.FSharp.Text.StructuredPrintfImpl.Layout Microsoft.FSharp.Text.StructuredPrintfImpl.Display/ObjectGraphFormatter::arrayValueL(System.Int32,System.Array)", "methodShortName": "arrayValueL(...)", "fileIndex": 0, "line": 1109,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 24, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "reportPath": "FSharp.Core_Display.html", "methodName": "Microsoft.FSharp.Text.StructuredPrintfImpl.Layout Microsoft.FSharp.Text.StructuredPrintfImpl.Display/possibleKeyValueL@1135-1::Invoke(o)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 1135,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 24, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.ReflectUtils", "reportPath": "FSharp.Core_ReflectUtils.html", "methodName": "Microsoft.FSharp.Text.StructuredPrintfImpl.ReflectUtils/ValueInfo Microsoft.FSharp.Text.StructuredPrintfImpl.ReflectUtils/Value::GetValueInfo(System.Reflection.BindingFlags,a,System.Type)", "methodShortName": "GetValueInfo(...)", "fileIndex": 0, "line": 517,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 24, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "ImageProcessing", "class": "ImageProcessing.Streaming", "reportPath": "ImageProcessing_Streaming.html", "methodName": "System.Void ImageProcessing.Streaming::processAllFiles(System.String,System.String,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Core.FSharpFunc`2<ImageProcessing.ImageProcessing/Image,ImageProcessing.ImageProcessing/Image>>)", "methodShortName": "processAllFiles(...)", "fileIndex": 0, "line": 58,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "Microsoft.FSharp.Core.FSharpValueOption`1<System.String> Microsoft.FSharp.Core.PrintfImpl/FormatString::parseInterpolatedHoleDotNetFormat(System.Char,System.String,System.Int32&)", "methodShortName": "parseInterpolatedHoleDotNetFormat(...)", "fileIndex": 0, "line": 204,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 45.03, "exceeded": true },
    ]},
  {
    "assembly": "AltCover.Monitor", "class": "AltCover.Carrier", "reportPath": "AltCover.Monitor_Carrier.html", "methodName": "System.Void AltCover.Carrier::Dispose(System.Boolean)", "methodShortName": "Dispose(...)", "fileIndex": 0, "line": 79,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Array2DModule", "reportPath": "FSharp.Core_Array2DModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.Array2DModule::Iterate(Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.Unit>,T[0...,0...])", "methodShortName": "Iterate(...)", "fileIndex": 0, "line": 92,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Array2DModule", "reportPath": "FSharp.Core_Array2DModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.Array2DModule::IterateIndexed(Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.Unit>>>,T[0...,0...])", "methodShortName": "IterateIndexed(...)", "fileIndex": 0, "line": 103,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[] Microsoft.FSharp.Collections.ArrayModule::Distinct(T[])", "methodShortName": "Distinct(...)", "fileIndex": 0, "line": 249,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "TState Microsoft.FSharp.Collections.ArrayModule::FoldBack2(Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,Microsoft.FSharp.Core.FSharpFunc`2<TState,TState>>>,T1[],T2[],TState)", "methodShortName": "FoldBack2(...)", "fileIndex": 0, "line": 917,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.ArrayModule::Fill(T[],System.Int32,System.Int32,T)", "methodShortName": "Fill(...)", "fileIndex": 0, "line": 1223,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[] Microsoft.FSharp.Collections.ArrayModule::Truncate(System.Int32,T[])", "methodShortName": "Truncate(...)", "fileIndex": 0, "line": 1267,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[] Microsoft.FSharp.Collections.ArrayModule::RemoveAt(System.Int32,T[])", "methodShortName": "RemoveAt(...)", "fileIndex": 0, "line": 1276,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[] Microsoft.FSharp.Collections.ArrayModule::RemoveManyAt(System.Int32,System.Int32,T[])", "methodShortName": "RemoveManyAt(...)", "fileIndex": 0, "line": 1290,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[] Microsoft.FSharp.Collections.ArrayModule::InsertAt(System.Int32,T,T[])", "methodShortName": "InsertAt(...)", "fileIndex": 0, "line": 1319,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.FSharpList<T>", "reportPath": "FSharp.Core_FSharpList_1.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<T> Microsoft.FSharp.Collections.FSharpList`1::GetSlice(Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "GetSlice(...)", "fileIndex": 0, "line": 3763,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.KeyCollection<T1, T2>", "reportPath": "FSharp.Core_KeyCollection_2.html", "methodName": "System.Void Microsoft.FSharp.Collections.KeyCollection`2::System.Collections.Generic.ICollection<'Key>.CopyTo(TKey[],System.Int32)", "methodShortName": "System.Collections.Generic.ICollection<'Key>.CopyTo(...)", "fileIndex": 0, "line": 836,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.KeyCollection<T1, T2>", "reportPath": "FSharp.Core_KeyCollection_2.html", "methodName": "System.Int32 <StartupCode$FSharp-Core>.$Map/System-Collections-Generic-IEnumerable<'Key>-GetEnumerator@851::GenerateNext(System.Collections.Generic.IEnumerable`1<TKey>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 0, "line": 851,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.KeyCollection<T1, T2>", "reportPath": "FSharp.Core_KeyCollection_2.html", "methodName": "System.Int32 <StartupCode$FSharp-Core>.$Map/System-Collections-IEnumerable-GetEnumerator@855::GenerateNext(System.Collections.Generic.IEnumerable`1<TKey>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 0, "line": 855,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "TState Microsoft.FSharp.Collections.ListModule::FoldBack(Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.FSharpFunc`2<TState,TState>>,Microsoft.FSharp.Collections.FSharpList`1<T>,TState)", "methodShortName": "FoldBack(...)", "fileIndex": 0, "line": 262,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 10, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<T> Microsoft.FSharp.Collections.SeqModule::TryFind(Microsoft.FSharp.Core.FSharpFunc`2<T,System.Boolean>,System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "TryFind(...)", "fileIndex": 1, "line": 666,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.SeqModule::IsEmpty(System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "IsEmpty(...)", "fileIndex": 1, "line": 697,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "T Microsoft.FSharp.Collections.SeqModule::Min(System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "Min(...)", "fileIndex": 1, "line": 1213,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "T Microsoft.FSharp.Collections.SeqModule::MinBy(Microsoft.FSharp.Core.FSharpFunc`2<T,TResult>,System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "MinBy(...)", "fileIndex": 1, "line": 1226,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "T Microsoft.FSharp.Collections.SeqModule::Max(System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "Max(...)", "fileIndex": 1, "line": 1260,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "T Microsoft.FSharp.Collections.SeqModule::MaxBy(Microsoft.FSharp.Core.FSharpFunc`2<T,TResult>,System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "MaxBy(...)", "fileIndex": 1, "line": 1273,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "T[] Microsoft.FSharp.Collections.SeqModule::nextChunk@1470(System.Int32,System.Collections.Generic.IEnumerator`1<T>,Microsoft.FSharp.Core.Unit)", "methodShortName": "nextChunk@1470(...)", "fileIndex": 1, "line": 1471,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule/ChunkBySize@1469::GenerateNext(System.Collections.Generic.IEnumerable`1<T[]>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 1, "line": 1469,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SetTreeModule", "reportPath": "FSharp.Core_SetTreeModule.html", "methodName": "Microsoft.FSharp.Collections.SetTree`1<T> Microsoft.FSharp.Collections.SetTreeModule::union(System.Collections.Generic.IComparer`1<T>,Microsoft.FSharp.Collections.SetTree`1<T>,Microsoft.FSharp.Collections.SetTree`1<T>)", "methodShortName": "union(...)", "fileIndex": 0, "line": 310,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ValueCollection<T1, T2>", "reportPath": "FSharp.Core_ValueCollection_2.html", "methodName": "System.Void Microsoft.FSharp.Collections.ValueCollection`2::System.Collections.Generic.ICollection<'Value>.CopyTo(TValue[],System.Int32)", "methodShortName": "System.Collections.Generic.ICollection<'Value>.CopyTo(...)", "fileIndex": 0, "line": 868,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ValueCollection<T1, T2>", "reportPath": "FSharp.Core_ValueCollection_2.html", "methodName": "System.Int32 <StartupCode$FSharp-Core>.$Map/System-Collections-Generic-IEnumerable<'Value>-GetEnumerator@883::GenerateNext(System.Collections.Generic.IEnumerable`1<TValue>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 0, "line": 883,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ValueCollection<T1, T2>", "reportPath": "FSharp.Core_ValueCollection_2.html", "methodName": "System.Int32 <StartupCode$FSharp-Core>.$Map/System-Collections-IEnumerable-GetEnumerator@887-1::GenerateNext(System.Collections.Generic.IEnumerable`1<TValue>&)", "methodShortName": "GenerateNext(...)", "fileIndex": 0, "line": 887,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "TResult Microsoft.FSharp.Core.LanguagePrimitives/BinaryOpDynamicImplTable`4::Invoke(System.String,T1,T2)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 2570,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Int32 Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::precheck@980(System.Array,System.Array,System.Int32,System.Int32)", "methodShortName": "precheck@980(...)", "fileIndex": 0, "line": 981,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Int32 Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::GenericComparisonByteArray(System.Byte[],System.Byte[])", "methodShortName": "GenericComparisonByteArray(...)", "fileIndex": 0, "line": 1022,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.MatchFailureException", "reportPath": "FSharp.Core_MatchFailureException.html", "methodName": "System.Boolean Microsoft.FSharp.Core.MatchFailureException::Equals(System.Object,System.Collections.IEqualityComparer)", "methodShortName": "Equals(...)", "fileIndex": 0, "line": 3264,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.MatchFailureException", "reportPath": "FSharp.Core_MatchFailureException.html", "methodName": "System.Boolean Microsoft.FSharp.Core.MatchFailureException::Equals(System.Exception)", "methodShortName": "Equals(...)", "fileIndex": 0, "line": 3264,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 10, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Int32 Microsoft.FSharp.Core.Operators/ArrayExtensions::[,,,]`1.GetReverseIndex(T[0...,0...,0...,0...],System.Int32,System.Int32)", "methodShortName": "[,,,]`1.GetReverseIndex(...)", "fileIndex": 0, "line": 6586,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 5, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.String Microsoft.FSharp.Core.Operators/OperatorIntrinsics::GetStringSlice(System.String,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "GetStringSlice(...)", "fileIndex": 0, "line": 5994,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Boolean Microsoft.FSharp.Core.Operators/OperatorIntrinsics/BaseRangeEnumerator`1::System.Collections.IEnumerator.MoveNext()", "methodShortName": "System.Collections.IEnumerator.MoveNext()", "fileIndex": 0, "line": 5192,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 12, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "System.String Microsoft.FSharp.Core.PrintfImpl/GenericNumber::rightJustifyWithZeroAsPadChar(System.String,System.Boolean,System.Boolean,System.Int32,System.String)", "methodShortName": "rightJustifyWithZeroAsPadChar(...)", "fileIndex": 0, "line": 678,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryBuilder", "reportPath": "FSharp.Core_QueryBuilder.html", "methodName": "System.Nullable`1<TValue> Microsoft.FSharp.Linq.QueryBuilder::AverageByNullable(Microsoft.FSharp.Linq.QuerySource`2<T,Q>,Microsoft.FSharp.Core.FSharpFunc`2<T,System.Nullable`1<TValue>>)", "methodShortName": "AverageByNullable(...)", "fileIndex": 0, "line": 174,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryBuilder", "reportPath": "FSharp.Core_QueryBuilder.html", "methodName": "System.Nullable`1<TValue> Microsoft.FSharp.Linq.QueryBuilder::AverageByNullable$W(Microsoft.FSharp.Core.FSharpFunc`2<TValue,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,TValue>>,Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Core.Unit,TValue>,Microsoft.FSharp.Core.FSharpFunc`2<TValue,Microsoft.FSharp.Core.FSharpFunc`2<TValue,TValue>>,Microsoft.FSharp.Linq.QuerySource`2<T,Q>,Microsoft.FSharp.Core.FSharpFunc`2<T,System.Nullable`1<TValue>>)", "methodShortName": "AverageByNullable$W(...)", "fileIndex": 0, "line": 174,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Tuple`2<Microsoft.FSharp.Quotations.FSharpVar,Microsoft.FSharp.Quotations.FSharpExpr>>,Microsoft.FSharp.Quotations.FSharpExpr> Microsoft.FSharp.Linq.QueryModule::stripSuccessiveProjLets@343-1(Microsoft.FSharp.Quotations.FSharpVar,System.Int32,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "stripSuccessiveProjLets@343-1(...)", "fileIndex": 0, "line": 346,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`4<Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Linq.QueryModule/|SpecificCall2|_|@328::Invoke(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 329,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.RuntimeHelpers.Adapters", "reportPath": "FSharp.Core_Adapters.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Linq.RuntimeHelpers.Adapters::SimplifyConsumingExpr(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "SimplifyConsumingExpr(...)", "fileIndex": 0, "line": 261,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter", "reportPath": "FSharp.Core_LeafExpressionConverter.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`2<System.Reflection.ConstructorInfo,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>> Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter::|NewAnonymousObjectQ|_|(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "|NewAnonymousObjectQ|_|(...)", "fileIndex": 0, "line": 259,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter", "reportPath": "FSharp.Core_LeafExpressionConverter.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`3<Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>> Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter/SpecificCallToMethodInfo@82::Invoke(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 86,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.Array", "reportPath": "FSharp.Core_Array.html", "methodName": "T[] Microsoft.FSharp.Primitives.Basics.Array::permute(Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,System.Int32>,T[])", "methodShortName": "permute(...)", "fileIndex": 0, "line": 1031,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "System.Void Microsoft.FSharp.Primitives.Basics.List::map3ToFreshConsTail(Microsoft.FSharp.Collections.FSharpList`1<a>,Microsoft.FSharp.Core.OptimizedClosures/FSharpFunc`4<b,c,d,a>,Microsoft.FSharp.Collections.FSharpList`1<b>,Microsoft.FSharp.Collections.FSharpList`1<c>,Microsoft.FSharp.Collections.FSharpList`1<d>,System.Int32)", "methodShortName": "map3ToFreshConsTail(...)", "fileIndex": 0, "line": 294,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<TResult> Microsoft.FSharp.Primitives.Basics.List::map3(Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,Microsoft.FSharp.Core.FSharpFunc`2<T3,TResult>>>,Microsoft.FSharp.Collections.FSharpList`1<T1>,Microsoft.FSharp.Collections.FSharpList`1<T2>,Microsoft.FSharp.Collections.FSharpList`1<T3>)", "methodShortName": "map3(...)", "fileIndex": 0, "line": 304,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<T> Microsoft.FSharp.Primitives.Basics.List::ofSeq(System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "ofSeq(...)", "fileIndex": 0, "line": 534,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "System.Void Microsoft.FSharp.Primitives.Basics.List::splitIntoToFreshConsTail(Microsoft.FSharp.Collections.FSharpList`1<a>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Collections.FSharpList`1<a>>,System.Int32,System.Int32,System.Int32,System.Int32,Microsoft.FSharp.Collections.FSharpList`1<a>)", "methodShortName": "splitIntoToFreshConsTail(...)", "fileIndex": 0, "line": 880,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "System.Void Microsoft.FSharp.Primitives.Basics.List::zip3ToFreshConsTail(Microsoft.FSharp.Collections.FSharpList`1<System.Tuple`3<a,b,c>>,Microsoft.FSharp.Collections.FSharpList`1<a>,Microsoft.FSharp.Collections.FSharpList`1<b>,Microsoft.FSharp.Collections.FSharpList`1<c>)", "methodShortName": "zip3ToFreshConsTail(...)", "fileIndex": 0, "line": 934,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<System.Tuple`3<T1,T2,T3>> Microsoft.FSharp.Primitives.Basics.List::zip3(Microsoft.FSharp.Collections.FSharpList`1<T1>,Microsoft.FSharp.Collections.FSharpList`1<T2>,Microsoft.FSharp.Collections.FSharpList`1<T3>)", "methodShortName": "zip3(...)", "fileIndex": 0, "line": 947,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.DerivedPatternsModule", "reportPath": "FSharp.Core_DerivedPatternsModule.html", "methodName": "System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpVar>,Microsoft.FSharp.Quotations.FSharpExpr> Microsoft.FSharp.Quotations.DerivedPatternsModule::stripSuccessiveProjLets@2155(Microsoft.FSharp.Quotations.FSharpVar,System.Int32,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "stripSuccessiveProjLets@2155(...)", "fileIndex": 0, "line": 2158,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.DerivedPatternsModule", "reportPath": "FSharp.Core_DerivedPatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`3<Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>>> Microsoft.FSharp.Quotations.DerivedPatternsModule::SpecificCallPattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "SpecificCallPattern(...)", "fileIndex": 0, "line": 2202,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.DerivedPatternsModule", "reportPath": "FSharp.Core_DerivedPatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`3<Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>> Microsoft.FSharp.Quotations.DerivedPatternsModule/SpecificCallPattern@2207-1::Invoke(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 2213,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.FSharpExpr", "reportPath": "FSharp.Core_FSharpExpr.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Quotations.FSharpExpr::Deserialize40(System.Type,System.Type[],System.Type[],Microsoft.FSharp.Quotations.FSharpExpr[],System.Byte[])", "methodShortName": "Deserialize40(...)", "fileIndex": 0, "line": 2092,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`4<Microsoft.FSharp.Quotations.ExprConstInfo,Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Quotations.PatternsModule::|Comb3|_|(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "|Comb3|_|(...)", "fileIndex": 0, "line": 412,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr> Microsoft.FSharp.Quotations.PatternsModule::QuoteRawPattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "QuoteRawPattern(...)", "fileIndex": 0, "line": 427,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr> Microsoft.FSharp.Quotations.PatternsModule::QuoteTypedPattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "QuoteTypedPattern(...)", "fileIndex": 0, "line": 430,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`2<Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Quotations.PatternsModule::AddressSetPattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "AddressSetPattern(...)", "fileIndex": 0, "line": 466,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`2<Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Quotations.PatternsModule::TryFinallyPattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "TryFinallyPattern(...)", "fileIndex": 0, "line": 469,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Tuple`2<Microsoft.FSharp.Quotations.FSharpVar,Microsoft.FSharp.Quotations.FSharpExpr>>,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Quotations.PatternsModule::LetRecursivePattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "LetRecursivePattern(...)", "fileIndex": 0, "line": 633,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Collections.FSharpSet`1<Microsoft.FSharp.Quotations.FSharpVar> Microsoft.FSharp.Quotations.PatternsModule::freeInExprAcc(Microsoft.FSharp.Collections.FSharpSet`1<Microsoft.FSharp.Quotations.FSharpVar>,Microsoft.FSharp.Collections.FSharpSet`1<Microsoft.FSharp.Quotations.FSharpVar>,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "freeInExprAcc(...)", "fileIndex": 0, "line": 1738,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`3<Microsoft.FSharp.Core.SourceConstructFlags,System.Int32,System.Int32>> Microsoft.FSharp.Reflection.Impl::tryFindCompilationMappingAttribute(System.Object[])", "methodShortName": "tryFindCompilationMappingAttribute(...)", "fileIndex": 0, "line": 247,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Boolean Microsoft.FSharp.Reflection.Impl::isUnionType(System.Type,System.Reflection.BindingFlags)", "methodShortName": "isUnionType(...)", "fileIndex": 0, "line": 393,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "reportPath": "FSharp.Core_Display.html", "methodName": "System.Int32 Microsoft.FSharp.Text.StructuredPrintfImpl.Display::addL@737-1(Microsoft.FSharp.Core.FSharpFunc`2<System.String,Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Collections.FSharpList`1<System.Tuple`2<System.String,System.String>>,Microsoft.FSharp.Core.FSharpFunc`2<System.Boolean,a>>>,Microsoft.FSharp.Core.FSharpFunc`2<System.Object,b>,Microsoft.FSharp.Text.StructuredPrintfImpl.TaggedTextWriter,System.Int32,System.Int32,Microsoft.FSharp.Text.StructuredPrintfImpl.Layout)", "methodShortName": "addL@737-1(...)", "fileIndex": 0, "line": 741,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.ReflectUtils", "reportPath": "FSharp.Core_ReflectUtils.html", "methodName": "Microsoft.FSharp.Text.StructuredPrintfImpl.ReflectUtils/ValueInfo Microsoft.FSharp.Text.StructuredPrintfImpl.ReflectUtils/Value::GetValueInfoOfObject$cont@494(System.Reflection.BindingFlags,System.Object,System.Type,Microsoft.FSharp.Core.Unit)", "methodShortName": "GetValueInfoOfObject$cont@494(...)", "fileIndex": 0, "line": 494,
    "metrics": [
      { "value": 6, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 42, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SetTreeModule", "reportPath": "FSharp.Core_SetTreeModule.html", "methodName": "Microsoft.FSharp.Collections.SetTree`1<a> Microsoft.FSharp.Collections.SetTreeModule::rebalance(Microsoft.FSharp.Collections.SetTree`1<a>,a,Microsoft.FSharp.Collections.SetTree`1<a>)", "methodShortName": "rebalance(...)", "fileIndex": 0, "line": 107,
    "metrics": [
      { "value": 9, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 33, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Array2DModule", "reportPath": "FSharp.Core_Array2DModule.html", "methodName": "T[0...,0...] Microsoft.FSharp.Collections.Array2DModule::CreateBased(System.Int32,System.Int32,System.Int32,System.Int32,T)", "methodShortName": "CreateBased(...)", "fileIndex": 0, "line": 66,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Array2DModule", "reportPath": "FSharp.Core_Array2DModule.html", "methodName": "T[0...,0...] Microsoft.FSharp.Collections.Array2DModule::InitializeBased(System.Int32,System.Int32,System.Int32,System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,T>>)", "methodShortName": "InitializeBased(...)", "fileIndex": 0, "line": 74,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Array4DModule", "reportPath": "FSharp.Core_Array4DModule.html", "methodName": "T[0...,0...,0...,0...] Microsoft.FSharp.Collections.Array4DModule::ZeroCreate(System.Int32,System.Int32,System.Int32,System.Int32)", "methodShortName": "ZeroCreate(...)", "fileIndex": 0, "line": 129,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[] Microsoft.FSharp.Collections.ArrayModule::Tail(T[])", "methodShortName": "Tail(...)", "fileIndex": 0, "line": 72,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Tuple`2<TKey,System.Int32>[] Microsoft.FSharp.Collections.ArrayModule::countByValueType(Microsoft.FSharp.Core.FSharpFunc`2<T,TKey>,T[])", "methodShortName": "countByValueType(...)", "fileIndex": 0, "line": 184,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Tuple`2<TKey,System.Int32>[] Microsoft.FSharp.Collections.ArrayModule::countByRefType(Microsoft.FSharp.Core.FSharpFunc`2<T,TKey>,T[])", "methodShortName": "countByRefType(...)", "fileIndex": 0, "line": 188,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.ArrayModule::Iterate2(Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,Microsoft.FSharp.Core.Unit>>,T1[],T2[])", "methodShortName": "Iterate2(...)", "fileIndex": 0, "line": 272,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "TResult[] Microsoft.FSharp.Collections.ArrayModule::Map2(Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,TResult>>,T1[],T2[])", "methodShortName": "Map2(...)", "fileIndex": 0, "line": 297,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "TResult[] Microsoft.FSharp.Collections.ArrayModule::MapIndexed2(Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,TResult>>>,T1[],T2[])", "methodShortName": "MapIndexed2(...)", "fileIndex": 0, "line": 322,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.ArrayModule::IterateIndexed2(Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,Microsoft.FSharp.Core.Unit>>>,T1[],T2[])", "methodShortName": "IterateIndexed2(...)", "fileIndex": 0, "line": 340,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Tuple`2<TKey,T[]>[] Microsoft.FSharp.Collections.ArrayModule::groupByImpl$cont@411(Microsoft.FSharp.Core.FSharpFunc`2<T,TKey>,T[],System.Collections.Generic.IEqualityComparer`1<TKey>,System.Int32,Microsoft.FSharp.Core.Unit)", "methodShortName": "groupByImpl$cont@411(...)", "fileIndex": 0, "line": 435,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Tuple`2<TKey,T[]>[] Microsoft.FSharp.Collections.ArrayModule::groupByImpl$cont@411-1(Microsoft.FSharp.Core.FSharpFunc`2<T,TKey>,T[],System.Collections.Generic.IEqualityComparer`1<Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers/StructBox`1<TKey>>,System.Int32,Microsoft.FSharp.Core.Unit)", "methodShortName": "groupByImpl$cont@411-1(...)", "fileIndex": 0, "line": 438,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[][] Microsoft.FSharp.Collections.ArrayModule::ChunkBySize(System.Int32,T[])", "methodShortName": "ChunkBySize(...)", "fileIndex": 0, "line": 791,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Tuple`2<T1,T2>[] Microsoft.FSharp.Collections.ArrayModule::Zip(T1[],T2[])", "methodShortName": "Zip(...)", "fileIndex": 0, "line": 816,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Tuple`2<T1,T2>[] Microsoft.FSharp.Collections.ArrayModule::AllPairs(T1[],T2[])", "methodShortName": "AllPairs(...)", "fileIndex": 0, "line": 839,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "TState Microsoft.FSharp.Collections.ArrayModule::Fold2(Microsoft.FSharp.Core.FSharpFunc`2<TState,Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,TState>>>,TState,T1[],T2[])", "methodShortName": "Fold2(...)", "fileIndex": 0, "line": 929,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Tuple`2<T,T>[] Microsoft.FSharp.Collections.ArrayModule::Pairwise(T[])", "methodShortName": "Pairwise(...)", "fileIndex": 0, "line": 971,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.ArrayModule::SortInPlaceWith(Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.FSharpFunc`2<T,System.Int32>>,T[])", "methodShortName": "SortInPlaceWith(...)", "fileIndex": 0, "line": 997,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T Microsoft.FSharp.Collections.ArrayModule::Min(T[])", "methodShortName": "Min(...)", "fileIndex": 0, "line": 1104,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T Microsoft.FSharp.Collections.ArrayModule::MinBy(Microsoft.FSharp.Core.FSharpFunc`2<T,TResult>,T[])", "methodShortName": "MinBy(...)", "fileIndex": 0, "line": 1115,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T Microsoft.FSharp.Collections.ArrayModule::Max(T[])", "methodShortName": "Max(...)", "fileIndex": 0, "line": 1129,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T Microsoft.FSharp.Collections.ArrayModule::MaxBy(Microsoft.FSharp.Core.FSharpFunc`2<T,TResult>,T[])", "methodShortName": "MaxBy(...)", "fileIndex": 0, "line": 1140,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[] Microsoft.FSharp.Collections.ArrayModule::UpdateAt(System.Int32,T,T[])", "methodShortName": "UpdateAt(...)", "fileIndex": 0, "line": 1305,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.FSharpList<T>", "reportPath": "FSharp.Core_FSharpList_1.html", "methodName": "System.String Microsoft.FSharp.Collections.FSharpList`1::ToString()", "methodShortName": "ToString()", "fileIndex": 0, "line": 3755,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.FSharpMap<T1, T2>", "reportPath": "FSharp.Core_FSharpMap_2.html", "methodName": "System.String Microsoft.FSharp.Collections.FSharpMap`2::ToString()", "methodShortName": "ToString()", "fileIndex": 0, "line": 790,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.FSharpSet<T>", "reportPath": "FSharp.Core_FSharpSet_1.html", "methodName": "System.String Microsoft.FSharp.Collections.FSharpSet`1::ToString()", "methodShortName": "ToString()", "fileIndex": 0, "line": 788,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Internal", "reportPath": "FSharp.Core_Internal.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.Internal/IEnumerator/upto@246::System.Collections.IEnumerator.MoveNext()", "methodShortName": "System.Collections.IEnumerator.MoveNext()", "fileIndex": 0, "line": 251,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.ListModule::forall2aux(Microsoft.FSharp.Core.OptimizedClosures/FSharpFunc`3<a,b,System.Boolean>,Microsoft.FSharp.Collections.FSharpList`1<a>,Microsoft.FSharp.Collections.FSharpList`1<b>)", "methodShortName": "forall2aux(...)", "fileIndex": 0, "line": 339,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.ListModule::exists2aux(Microsoft.FSharp.Core.OptimizedClosures/FSharpFunc`3<a,b,System.Boolean>,Microsoft.FSharp.Collections.FSharpList`1<a>,Microsoft.FSharp.Collections.FSharpList`1<b>)", "methodShortName": "exists2aux(...)", "fileIndex": 0, "line": 368,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<T> Microsoft.FSharp.Collections.ListModule::RemoveAt(System.Int32,Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "RemoveAt(...)", "fileIndex": 0, "line": 698,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<T> Microsoft.FSharp.Collections.ListModule::RemoveManyAt(System.Int32,System.Int32,Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "RemoveManyAt(...)", "fileIndex": 0, "line": 715,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<T> Microsoft.FSharp.Collections.ListModule::UpdateAt(System.Int32,T,Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "UpdateAt(...)", "fileIndex": 0, "line": 731,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.ListModule/loop@659-32::Invoke(Microsoft.FSharp.Collections.FSharpList`1<T>,Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 661,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.MapTreeModule", "reportPath": "FSharp.Core_MapTreeModule.html", "methodName": "Microsoft.FSharp.Collections.MapTree`2<TKey,TValue> Microsoft.FSharp.Collections.MapTreeModule::mk(Microsoft.FSharp.Collections.MapTree`2<TKey,TValue>,TKey,TValue,Microsoft.FSharp.Collections.MapTree`2<TKey,TValue>)", "methodShortName": "mk(...)", "fileIndex": 0, "line": 99,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.MapTreeModule", "reportPath": "FSharp.Core_MapTreeModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.MapTreeModule::tryGetValue(System.Collections.Generic.IComparer`1<TKey>,TKey,TValue&,Microsoft.FSharp.Collections.MapTree`2<TKey,TValue>)", "methodShortName": "tryGetValue(...)", "fileIndex": 0, "line": 145,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.MapTreeModule", "reportPath": "FSharp.Core_MapTreeModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.MapTreeModule::mem(System.Collections.Generic.IComparer`1<TKey>,TKey,Microsoft.FSharp.Collections.MapTree`2<TKey,TValue>)", "methodShortName": "mem(...)", "fileIndex": 0, "line": 274,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.MapTreeModule", "reportPath": "FSharp.Core_MapTreeModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<a> Microsoft.FSharp.Collections.MapTreeModule::tryPickOpt(Microsoft.FSharp.Core.OptimizedClosures/FSharpFunc`3<TKey,TValue,Microsoft.FSharp.Core.FSharpOption`1<a>>,Microsoft.FSharp.Collections.MapTree`2<TKey,TValue>)", "methodShortName": "tryPickOpt(...)", "fileIndex": 0, "line": 299,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.MapTreeModule", "reportPath": "FSharp.Core_MapTreeModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.MapTreeModule::existsOpt(Microsoft.FSharp.Core.OptimizedClosures/FSharpFunc`3<TKey,TValue,System.Boolean>,Microsoft.FSharp.Collections.MapTree`2<TKey,TValue>)", "methodShortName": "existsOpt(...)", "fileIndex": 0, "line": 318,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.MapTreeModule", "reportPath": "FSharp.Core_MapTreeModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.MapTreeModule::forallOpt(Microsoft.FSharp.Core.OptimizedClosures/FSharpFunc`3<TKey,TValue,System.Boolean>,Microsoft.FSharp.Collections.MapTree`2<TKey,TValue>)", "methodShortName": "forallOpt(...)", "fileIndex": 0, "line": 331,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.SeqModule::Exists(Microsoft.FSharp.Core.FSharpFunc`2<T,System.Boolean>,System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "Exists(...)", "fileIndex": 1, "line": 529,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.SeqModule::Contains(T,System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "Contains(...)", "fileIndex": 1, "line": 538,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.SeqModule::ForAll(Microsoft.FSharp.Core.FSharpFunc`2<T,System.Boolean>,System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "ForAll(...)", "fileIndex": 1, "line": 547,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<TResult> Microsoft.FSharp.Collections.SeqModule::TryPick(Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.FSharpOption`1<TResult>>,System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "TryPick(...)", "fileIndex": 1, "line": 650,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "T Microsoft.FSharp.Collections.SeqModule::Reduce(Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.FSharpFunc`2<T,T>>,System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "Reduce(...)", "fileIndex": 1, "line": 753,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SeqModule::CompareWith(Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.FSharpFunc`2<T,System.Int32>>,System.Collections.Generic.IEnumerable`1<T>,System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "CompareWith(...)", "fileIndex": 1, "line": 781,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule::oneStepTo@987(System.Collections.Generic.IEnumerable`1<T>,System.Collections.Generic.List`1<T>,Microsoft.FSharp.Core.FSharpRef`1<Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Core.FSharpOption`1<System.Collections.Generic.IEnumerator`1<T>>>>,System.Int32)", "methodShortName": "oneStepTo@987(...)", "fileIndex": 1, "line": 990,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "T Microsoft.FSharp.Collections.SeqModule::Average(System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "Average(...)", "fileIndex": 1, "line": 1187,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "T Microsoft.FSharp.Collections.SeqModule::Average$W(Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,T>>,Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Core.Unit,T>,Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.FSharpFunc`2<T,T>>,System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "Average$W(...)", "fileIndex": 1, "line": 1187,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "TResult Microsoft.FSharp.Collections.SeqModule::AverageBy(Microsoft.FSharp.Core.FSharpFunc`2<T,TResult>,System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "AverageBy(...)", "fileIndex": 1, "line": 1200,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "TResult Microsoft.FSharp.Collections.SeqModule::AverageBy$W(Microsoft.FSharp.Core.FSharpFunc`2<TResult,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,TResult>>,Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Core.Unit,TResult>,Microsoft.FSharp.Core.FSharpFunc`2<TResult,Microsoft.FSharp.Core.FSharpFunc`2<TResult,TResult>>,Microsoft.FSharp.Core.FSharpFunc`2<T,TResult>,System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "AverageBy$W(...)", "fileIndex": 1, "line": 1200,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "T Microsoft.FSharp.Collections.SeqModule::ExactlyOne(System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "ExactlyOne(...)", "fileIndex": 1, "line": 1399,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<T> Microsoft.FSharp.Collections.SeqModule::TryExactlyOne(System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "TryExactlyOne(...)", "fileIndex": 1, "line": 1412,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SetTreeModule", "reportPath": "FSharp.Core_SetTreeModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.SetTreeModule::forall(Microsoft.FSharp.Core.FSharpFunc`2<T,System.Boolean>,Microsoft.FSharp.Collections.SetTree`1<T>)", "methodShortName": "forall(...)", "fileIndex": 0, "line": 263,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SetTreeModule", "reportPath": "FSharp.Core_SetTreeModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.SetTreeModule::exists(Microsoft.FSharp.Core.FSharpFunc`2<T,System.Boolean>,Microsoft.FSharp.Collections.SetTree`1<T>)", "methodShortName": "exists(...)", "fileIndex": 0, "line": 271,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SetTreeModule", "reportPath": "FSharp.Core_SetTreeModule.html", "methodName": "Microsoft.FSharp.Collections.SetTree`1<T> Microsoft.FSharp.Collections.SetTreeModule::filterAux(System.Collections.Generic.IComparer`1<T>,Microsoft.FSharp.Core.FSharpFunc`2<T,System.Boolean>,Microsoft.FSharp.Collections.SetTree`1<T>,Microsoft.FSharp.Collections.SetTree`1<T>)", "methodShortName": "filterAux(...)", "fileIndex": 0, "line": 285,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.AsyncPrimitives", "reportPath": "FSharp.Core_AsyncPrimitives.html", "methodName": "Microsoft.FSharp.Control.AsyncReturn Microsoft.FSharp.Control.AsyncPrimitives::CallFilterThenInvoke(Microsoft.FSharp.Control.AsyncActivation`1<T>,Microsoft.FSharp.Core.FSharpFunc`2<System.Exception,Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Control.FSharpAsync`1<T>>>,System.Runtime.ExceptionServices.ExceptionDispatchInfo)", "methodShortName": "CallFilterThenInvoke(...)", "fileIndex": 0, "line": 482,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.FSharpAsync", "reportPath": "FSharp.Core_FSharpAsync.html", "methodName": "Microsoft.FSharp.Control.AsyncReturn <StartupCode$FSharp-Core>.$Async/scont@1515::Invoke(Microsoft.FSharp.Core.FSharpOption`1<T>)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 1529,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.FSharpAsync", "reportPath": "FSharp.Core_FSharpAsync.html", "methodName": "Microsoft.FSharp.Control.AsyncReturn <StartupCode$FSharp-Core>.$Async/AwaitWaitHandle@1636::Invoke(Microsoft.FSharp.Control.AsyncActivation`1<System.Boolean>)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 1637,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.Mailbox<T>", "reportPath": "FSharp.Core_Mailbox_1.html", "methodName": "Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.FSharpOption`1<T>> <StartupCode$FSharp-Core>.$Mailbox/scan@190-2::Invoke(Microsoft.FSharp.Core.FSharpChoice`2<System.Boolean,Microsoft.FSharp.Core.Unit>)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 192,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.Mailbox<T>", "reportPath": "FSharp.Core_Mailbox_1.html", "methodName": "Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.FSharpOption`1<TMsg>> <StartupCode$FSharp-Core>.$Mailbox/processFirstArrival@257-1::Invoke(Microsoft.FSharp.Core.Unit)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 257,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.Mailbox<T>", "reportPath": "FSharp.Core_Mailbox_1.html", "methodName": "Microsoft.FSharp.Control.FSharpAsync`1<TMsg> <StartupCode$FSharp-Core>.$Mailbox/processFirstArrival@287-7::Invoke(Microsoft.FSharp.Core.Unit)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 287,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.CompilerServices.ArrayCollector<T>", "reportPath": "FSharp.Core_ArrayCollector_1.html", "methodName": "System.Void Microsoft.FSharp.Core.CompilerServices.ArrayCollector`1::Add(T)", "methodShortName": "Add(...)", "fileIndex": 0, "line": 485,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.CompilerServices.ArrayCollector<T>", "reportPath": "FSharp.Core_ArrayCollector_1.html", "methodName": "T[] Microsoft.FSharp.Core.CompilerServices.ArrayCollector`1::Close()", "methodShortName": "Close()", "fileIndex": 0, "line": 523,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Int32 Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::GenericComparisonObjArrayWithComparer(Microsoft.FSharp.Core.LanguagePrimitives/HashCompare/GenericComparer,System.Object[],System.Object[])", "methodShortName": "GenericComparisonObjArrayWithComparer(...)", "fileIndex": 0, "line": 1007,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Int32 Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::GenericHashArbArray(System.Collections.IEqualityComparer,System.Array)", "methodShortName": "GenericHashArbArray(...)", "fileIndex": 0, "line": 1673,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Boolean Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::FastEqualsTuple5(System.Collections.IEqualityComparer,System.Tuple`5<T1,T2,T3,T4,T5>,System.Tuple`5<T1,T2,T3,T4,T5>)", "methodShortName": "FastEqualsTuple5(...)", "fileIndex": 0, "line": 1927,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Int32 Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::FastCompareTuple5(System.Collections.IComparer,System.Tuple`5<T1,T2,T3,T4,T5>,System.Tuple`5<T1,T2,T3,T4,T5>)", "methodShortName": "FastCompareTuple5(...)", "fileIndex": 0, "line": 1983,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Int32 Microsoft.FSharp.Core.Operators/ArrayExtensions::[,,]`1.GetReverseIndex(T[0...,0...,0...],System.Int32,System.Int32)", "methodShortName": "[,,]`1.GetReverseIndex(...)", "fileIndex": 0, "line": 6598,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Void Microsoft.FSharp.Core.Operators/OperatorIntrinsics::SetArraySlice(T[],Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>,T[])", "methodShortName": "SetArraySlice(...)", "fileIndex": 0, "line": 5571,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.OptimizedClosures", "reportPath": "FSharp.Core_OptimizedClosures.html", "methodName": "Microsoft.FSharp.Core.OptimizedClosures/FSharpFunc`6<T1,T2,T3,T4,T5,TResult> Microsoft.FSharp.Core.OptimizedClosures/FSharpFunc`6::Adapt(Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,Microsoft.FSharp.Core.FSharpFunc`2<T3,Microsoft.FSharp.Core.FSharpFunc`2<T4,Microsoft.FSharp.Core.FSharpFunc`2<T5,TResult>>>>>)", "methodShortName": "Adapt(...)", "fileIndex": 0, "line": 3340,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "System.Object Microsoft.FSharp.Core.PrintfImpl/Integer::eliminateNative(System.Object)", "methodShortName": "eliminateNative(...)", "fileIndex": 0, "line": 733,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.NullableOperators", "reportPath": "FSharp.Core_NullableOperators.html", "methodName": "System.Boolean Microsoft.FSharp.Linq.NullableOperators::op_QmarkEqualsQmark(System.Nullable`1<T>,System.Nullable`1<T>)", "methodShortName": "op_QmarkEqualsQmark(...)", "fileIndex": 0, "line": 44,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryBuilder", "reportPath": "FSharp.Core_QueryBuilder.html", "methodName": "System.Nullable`1<TValue> Microsoft.FSharp.Linq.QueryBuilder::SumByNullable(Microsoft.FSharp.Linq.QuerySource`2<T,Q>,Microsoft.FSharp.Core.FSharpFunc`2<T,System.Nullable`1<TValue>>)", "methodShortName": "SumByNullable(...)", "fileIndex": 0, "line": 153,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryBuilder", "reportPath": "FSharp.Core_QueryBuilder.html", "methodName": "System.Nullable`1<TValue> Microsoft.FSharp.Linq.QueryBuilder::SumByNullable$W(Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Core.Unit,TValue>,Microsoft.FSharp.Core.FSharpFunc`2<TValue,Microsoft.FSharp.Core.FSharpFunc`2<TValue,TValue>>,Microsoft.FSharp.Linq.QuerySource`2<T,Q>,Microsoft.FSharp.Core.FSharpFunc`2<T,System.Nullable`1<TValue>>)", "methodShortName": "SumByNullable$W(...)", "fileIndex": 0, "line": 153,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryBuilder", "reportPath": "FSharp.Core_QueryBuilder.html", "methodName": "TValue Microsoft.FSharp.Linq.QueryBuilder::AverageBy(Microsoft.FSharp.Linq.QuerySource`2<T,Q>,Microsoft.FSharp.Core.FSharpFunc`2<T,TValue>)", "methodShortName": "AverageBy(...)", "fileIndex": 0, "line": 193,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryBuilder", "reportPath": "FSharp.Core_QueryBuilder.html", "methodName": "TValue Microsoft.FSharp.Linq.QueryBuilder::AverageBy$W(Microsoft.FSharp.Core.FSharpFunc`2<TValue,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,TValue>>,Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Core.Unit,TValue>,Microsoft.FSharp.Core.FSharpFunc`2<TValue,Microsoft.FSharp.Core.FSharpFunc`2<TValue,TValue>>,Microsoft.FSharp.Linq.QuerySource`2<T,Q>,Microsoft.FSharp.Core.FSharpFunc`2<T,TValue>)", "methodShortName": "AverageBy$W(...)", "fileIndex": 0, "line": 193,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr> Microsoft.FSharp.Linq.QueryModule::|MacroReduction|_|(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "|MacroReduction|_|(...)", "fileIndex": 0, "line": 990,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Linq.QueryModule::TransInnerWithFinalConsume(Microsoft.FSharp.Linq.QueryModule/CanEliminate,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "TransInnerWithFinalConsume(...)", "fileIndex": 0, "line": 1662,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`3<Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Linq.QueryModule/|SpecificCall1|_|@321::Invoke(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 322,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Linq.QueryModule/MakeSelect@750::Invoke(System.Tuple`5<Microsoft.FSharp.Linq.QueryModule/CanEliminate,System.Boolean,Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpVar,Microsoft.FSharp.Quotations.FSharpExpr>)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 754,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<System.Tuple`2<Microsoft.FSharp.Quotations.FSharpVar,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Linq.QueryModule/tab@1004-1::Invoke(Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpVar>,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 1006,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "Microsoft.FSharp.Linq.QueryModule/TransInnerResult Microsoft.FSharp.Linq.QueryModule/TransInnerResult::MakeSelect(Microsoft.FSharp.Linq.QueryModule/CanEliminate,System.Boolean,Microsoft.FSharp.Linq.QueryModule/TransInnerResult,Microsoft.FSharp.Quotations.FSharpVar,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "MakeSelect(...)", "fileIndex": 0, "line": 1249,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.RuntimeHelpers.Adapters", "reportPath": "FSharp.Core_Adapters.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`3<Microsoft.FSharp.Quotations.FSharpVar,Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>> Microsoft.FSharp.Linq.RuntimeHelpers.Adapters::|ObjectConstruction|_|(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "|ObjectConstruction|_|(...)", "fileIndex": 0, "line": 86,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter", "reportPath": "FSharp.Core_LeafExpressionConverter.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`2<Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>> Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter::|MemberInitializationQ|_|(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "|MemberInitializationQ|_|(...)", "fileIndex": 0, "line": 254,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter", "reportPath": "FSharp.Core_LeafExpressionConverter.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr> Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter::|NullableConstruction|_|(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "|NullableConstruction|_|(...)", "fileIndex": 0, "line": 264,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.Array", "reportPath": "FSharp.Core_Array.html", "methodName": "System.Tuple`2<TResult[],TState> Microsoft.FSharp.Primitives.Basics.Array::mapFold(Microsoft.FSharp.Core.FSharpFunc`2<TState,Microsoft.FSharp.Core.FSharpFunc`2<T,System.Tuple`2<TResult,TState>>>,TState,T[])", "methodShortName": "mapFold(...)", "fileIndex": 0, "line": 1043,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.Array", "reportPath": "FSharp.Core_Array.html", "methodName": "System.Tuple`2<TResult[],TState> Microsoft.FSharp.Primitives.Basics.Array::mapFoldBack(Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.FSharpFunc`2<TState,System.Tuple`2<TResult,TState>>>,T[],TState)", "methodShortName": "mapFoldBack(...)", "fileIndex": 0, "line": 1056,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<T>,Microsoft.FSharp.Collections.FSharpList`1<T>> Microsoft.FSharp.Primitives.Basics.List::partition(Microsoft.FSharp.Core.FSharpFunc`2<T,System.Boolean>,Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "partition(...)", "fileIndex": 0, "line": 664,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Collections.FSharpList`1<T>> Microsoft.FSharp.Primitives.Basics.List::transpose(Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Collections.FSharpList`1<T>>)", "methodShortName": "transpose(...)", "fileIndex": 0, "line": 731,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Collections.FSharpList`1<T>> Microsoft.FSharp.Primitives.Basics.List::splitInto(System.Int32,Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "splitInto(...)", "fileIndex": 0, "line": 894,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<T> Microsoft.FSharp.Primitives.Basics.List::takeWhile(Microsoft.FSharp.Core.FSharpFunc`2<T,System.Boolean>,Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "takeWhile(...)", "fileIndex": 0, "line": 968,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.DerivedPatternsModule", "reportPath": "FSharp.Core_DerivedPatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Core.Unit> Microsoft.FSharp.Quotations.DerivedPatternsModule::UnitPattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "UnitPattern(...)", "fileIndex": 0, "line": 2149,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`3<Microsoft.FSharp.Quotations.ExprConstInfo,Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Quotations.PatternsModule::|Comb2|_|(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "|Comb2|_|(...)", "fileIndex": 0, "line": 410,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr> Microsoft.FSharp.Quotations.PatternsModule::QuotePattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "QuotePattern(...)", "fileIndex": 0, "line": 424,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`3<System.Object,System.Type,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Quotations.PatternsModule::WithValuePattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "WithValuePattern(...)", "fileIndex": 0, "line": 492,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`3<Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr>,System.Reflection.PropertyInfo,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>> Microsoft.FSharp.Quotations.PatternsModule::PropertyGetPattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "PropertyGetPattern(...)", "fileIndex": 0, "line": 522,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "System.Int32 Microsoft.FSharp.Quotations.PatternsModule::iter@1151-1(System.Type[],System.Type[],System.Reflection.ParameterInfo[],System.Int32,System.Int32)", "methodShortName": "iter@1151-1(...)", "fileIndex": 0, "line": 1152,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,System.Type> Microsoft.FSharp.Quotations.PatternsModule::u_tyconstSpec(Microsoft.FSharp.Quotations.PatternsModule/SimpleUnpickle/InputState)", "methodShortName": "u_tyconstSpec(...)", "fileIndex": 0, "line": 1437,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr> Microsoft.FSharp.Quotations.PatternsModule::tryGetReflectedDefinitionInstantiated(System.Reflection.MethodBase)", "methodShortName": "tryGetReflectedDefinitionInstantiated(...)", "fileIndex": 0, "line": 1883,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.FSharpValue", "reportPath": "FSharp.Core_FSharpValue.html", "methodName": "System.Object Microsoft.FSharp.Reflection.FSharpValue::MakeFunction(System.Type,Microsoft.FSharp.Core.FSharpFunc`2<System.Object,System.Object>)", "methodShortName": "MakeFunction(...)", "fileIndex": 0, "line": 1033,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.FSharpValue", "reportPath": "FSharp.Core_FSharpValue.html", "methodName": "System.Object Microsoft.FSharp.Reflection.FSharpValue::GetTupleField(System.Object,System.Int32)", "methodShortName": "GetTupleField(...)", "fileIndex": 0, "line": 1057,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Reflection.MethodInfo Microsoft.FSharp.Reflection.Impl::getUnionCaseConstructorMethod(System.Type,System.Int32,System.Reflection.BindingFlags)", "methodShortName": "getUnionCaseConstructorMethod(...)", "fileIndex": 0, "line": 483,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Object[] Microsoft.FSharp.Reflection.Impl/getTupleReader@707::Invoke(System.Object)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 708,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "reportPath": "FSharp.Core_Display.html", "methodName": "System.Boolean Microsoft.FSharp.Text.StructuredPrintfImpl.Display::isSetOrMapType(System.Type)", "methodShortName": "isSetOrMapType(...)", "fileIndex": 0, "line": 868,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.ReflectUtils", "reportPath": "FSharp.Core_ReflectUtils.html", "methodName": "Microsoft.FSharp.Text.StructuredPrintfImpl.ReflectUtils/ValueInfo Microsoft.FSharp.Text.StructuredPrintfImpl.ReflectUtils/Value::GetValueInfoOfObject(System.Reflection.BindingFlags,System.Object)", "methodShortName": "GetValueInfoOfObject(...)", "fileIndex": 0, "line": 469,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "ImageProcessing", "class": "ImageProcessing.ImageProcessing", "reportPath": "ImageProcessing_ImageProcessing.html", "methodName": "System.Byte[0...,0...] ImageProcessing.ImageProcessing::loadAs2DArray(System.String)", "methodShortName": "loadAs2DArray(...)", "fileIndex": 0, "line": 22,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 30, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "System.Object Microsoft.FSharp.Core.PrintfImpl/FormatParser`4::parseAndCreateFunctionFactory()", "methodShortName": "parseAndCreateFunctionFactory()", "fileIndex": 0, "line": 1253,
    "metrics": [
      { "value": 13, "exceeded": false },
      { "value": 192, "exceeded": false },
      { "value": 23.82, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<T> Microsoft.FSharp.Primitives.Basics.List::filter(Microsoft.FSharp.Core.FSharpFunc`2<T,System.Boolean>,Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "filter(...)", "fileIndex": 0, "line": 486,
    "metrics": [
      { "value": 5, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 22.56, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "System.Void Microsoft.FSharp.Core.PrintfImpl/FormatString::skipInterpolationHole(System.Char,System.String,System.Int32&)", "methodShortName": "skipInterpolationHole(...)", "fileIndex": 0, "line": 220,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 72, "exceeded": false },
      { "value": 21.82, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Array3DModule", "reportPath": "FSharp.Core_Array3DModule.html", "methodName": "T[0...,0...,0...] Microsoft.FSharp.Collections.Array3DModule::ZeroCreate(System.Int32,System.Int32,System.Int32)", "methodShortName": "ZeroCreate(...)", "fileIndex": 0, "line": 37,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.ArrayModule::Contains(T,T[])", "methodShortName": "Contains(...)", "fileIndex": 0, "line": 374,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.ArrayModule::Exists2(Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,System.Boolean>>,T1[],T2[])", "methodShortName": "Exists2(...)", "fileIndex": 0, "line": 385,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.ArrayModule::ForAll2(Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,System.Boolean>>,T1[],T2[])", "methodShortName": "ForAll2(...)", "fileIndex": 0, "line": 402,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T[] Microsoft.FSharp.Collections.ArrayModule::Except(System.Collections.Generic.IEnumerable`1<T>,T[])", "methodShortName": "Except(...)", "fileIndex": 0, "line": 689,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "TState Microsoft.FSharp.Collections.ArrayModule::FoldBack(Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.FSharpFunc`2<TState,TState>>,T[],TState)", "methodShortName": "FoldBack(...)", "fileIndex": 0, "line": 907,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "b Microsoft.FSharp.Collections.ArrayModule::foldSubRight(Microsoft.FSharp.Core.FSharpFunc`2<a,Microsoft.FSharp.Core.FSharpFunc`2<b,b>>,a[],System.Int32,System.Int32,b)", "methodShortName": "foldSubRight(...)", "fileIndex": 0, "line": 938,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "a[] Microsoft.FSharp.Collections.ArrayModule::scanSubLeft(Microsoft.FSharp.Core.FSharpFunc`2<a,Microsoft.FSharp.Core.FSharpFunc`2<b,a>>,a,b[],System.Int32,System.Int32)", "methodShortName": "scanSubLeft(...)", "fileIndex": 0, "line": 946,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T Microsoft.FSharp.Collections.ArrayModule::Reduce(Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.FSharpFunc`2<T,T>>,T[])", "methodShortName": "Reduce(...)", "fileIndex": 0, "line": 977,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T Microsoft.FSharp.Collections.ArrayModule::Average(T[])", "methodShortName": "Average(...)", "fileIndex": 0, "line": 1154,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T Microsoft.FSharp.Collections.ArrayModule::Average$W(Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,T>>,Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Core.Unit,T>,Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.FSharpFunc`2<T,T>>,T[])", "methodShortName": "Average$W(...)", "fileIndex": 0, "line": 1154,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "TResult Microsoft.FSharp.Collections.ArrayModule::AverageBy(Microsoft.FSharp.Core.FSharpFunc`2<T,TResult>,T[])", "methodShortName": "AverageBy(...)", "fileIndex": 0, "line": 1163,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "TResult Microsoft.FSharp.Collections.ArrayModule::AverageBy$W(Microsoft.FSharp.Core.FSharpFunc`2<TResult,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,TResult>>,Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Core.Unit,TResult>,Microsoft.FSharp.Core.FSharpFunc`2<TResult,Microsoft.FSharp.Core.FSharpFunc`2<TResult,TResult>>,Microsoft.FSharp.Core.FSharpFunc`2<T,TResult>,T[])", "methodShortName": "AverageBy$W(...)", "fileIndex": 0, "line": 1163,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<T> Microsoft.FSharp.Collections.ArrayModule::TryItem(System.Int32,T[])", "methodShortName": "TryItem(...)", "fileIndex": 0, "line": 1209,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "T Microsoft.FSharp.Collections.ArrayModule::ExactlyOne(T[])", "methodShortName": "ExactlyOne(...)", "fileIndex": 0, "line": 1231,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "TResult[] Microsoft.FSharp.Collections.ArrayModule/Parallel::Choose(Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.FSharpOption`1<TResult>>,T[])", "methodShortName": "Choose(...)", "fileIndex": 0, "line": 1361,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "System.Tuple`2<T[],T[]> Microsoft.FSharp.Collections.ArrayModule/Parallel::Partition(Microsoft.FSharp.Core.FSharpFunc`2<T,System.Boolean>,T[])", "methodShortName": "Partition(...)", "fileIndex": 0, "line": 1435,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ArrayModule", "reportPath": "FSharp.Core_ArrayModule.html", "methodName": "a[] Microsoft.FSharp.Collections.ArrayModule/Filter::filterViaMask(System.UInt32[],System.UInt32,System.Int32,a[])", "methodShortName": "filterViaMask(...)", "fileIndex": 0, "line": 652,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.FSharpMap<T1, T2>", "reportPath": "FSharp.Core_FSharpMap_2.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.FSharpMap`2::Equals(System.Object)", "methodShortName": "Equals(...)", "fileIndex": 0, "line": 710,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.FSharpSet<T>", "reportPath": "FSharp.Core_FSharpSet_1.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.FSharpSet`1::Equals(System.Object)", "methodShortName": "Equals(...)", "fileIndex": 0, "line": 739,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Generator", "reportPath": "FSharp.Core_Generator.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.Generator/EnumeratorWrappingLazyGenerator`1::System.Collections.IEnumerator.MoveNext()", "methodShortName": "System.Collections.IEnumerator.MoveNext()", "fileIndex": 0, "line": 399,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Internal", "reportPath": "FSharp.Core_Internal.html", "methodName": "T Microsoft.FSharp.Collections.Internal/IEnumerator::nth(System.Int32,System.Collections.Generic.IEnumerator`1<T>)", "methodShortName": "nth(...)", "fileIndex": 0, "line": 26,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Internal", "reportPath": "FSharp.Core_Internal.html", "methodName": "TResult Microsoft.FSharp.Collections.Internal/IEnumerator::getCurrent@238(Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,TResult>,Microsoft.FSharp.Core.FSharpRef`1<System.Int32>,Microsoft.FSharp.Core.FSharpRef`1<System.Lazy`1<TResult>>,Microsoft.FSharp.Core.Unit)", "methodShortName": "getCurrent@238(...)", "fileIndex": 0, "line": 239,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.Internal", "reportPath": "FSharp.Core_Internal.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.Internal/IEnumerator/map3@141::DoMoveNext(d&)", "methodShortName": "DoMoveNext(...)", "fileIndex": 0, "line": 143,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "T Microsoft.FSharp.Collections.ListModule::Item(System.Int32,Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "Item(...)", "fileIndex": 0, "line": 137,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<T> Microsoft.FSharp.Collections.ListModule::TryItem(System.Int32,Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "TryItem(...)", "fileIndex": 0, "line": 145,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<T> Microsoft.FSharp.Collections.ListModule::Replicate(System.Int32,T)", "methodShortName": "Replicate(...)", "fileIndex": 0, "line": 175,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.ListModule::loop@184-26(Microsoft.FSharp.Core.OptimizedClosures/FSharpFunc`3<T1,T2,Microsoft.FSharp.Core.Unit>,Microsoft.FSharp.Collections.FSharpList`1<T1>,Microsoft.FSharp.Collections.FSharpList`1<T2>)", "methodShortName": "loop@184-26(...)", "fileIndex": 0, "line": 186,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.ListModule::loop@195-27(Microsoft.FSharp.Core.OptimizedClosures/FSharpFunc`4<System.Int32,T1,T2,Microsoft.FSharp.Core.Unit>,System.Int32,Microsoft.FSharp.Collections.FSharpList`1<T1>,Microsoft.FSharp.Collections.FSharpList`1<T2>)", "methodShortName": "loop@195-27(...)", "fileIndex": 0, "line": 197,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "TState Microsoft.FSharp.Collections.ListModule::loop@245-28(Microsoft.FSharp.Core.OptimizedClosures/FSharpFunc`4<TState,T1,T2,TState>,TState,Microsoft.FSharp.Collections.FSharpList`1<T1>,Microsoft.FSharp.Collections.FSharpList`1<T2>)", "methodShortName": "loop@245-28(...)", "fileIndex": 0, "line": 247,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "c Microsoft.FSharp.Collections.ListModule::foldBack2UsingArrays(Microsoft.FSharp.Core.OptimizedClosures/FSharpFunc`4<a,b,c,c>,Microsoft.FSharp.Collections.FSharpList`1<a>,Microsoft.FSharp.Collections.FSharpList`1<b>,c)", "methodShortName": "foldBack2UsingArrays(...)", "fileIndex": 0, "line": 309,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "TState Microsoft.FSharp.Collections.ListModule::FoldBack2(Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,Microsoft.FSharp.Core.FSharpFunc`2<TState,TState>>>,Microsoft.FSharp.Collections.FSharpList`1<T1>,Microsoft.FSharp.Collections.FSharpList`1<T2>,TState)", "methodShortName": "FoldBack2(...)", "fileIndex": 0, "line": 325,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "T Microsoft.FSharp.Collections.ListModule::Max(Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "Max(...)", "fileIndex": 0, "line": 579,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "T Microsoft.FSharp.Collections.ListModule::MaxBy(Microsoft.FSharp.Core.FSharpFunc`2<T,TResult>,Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "MaxBy(...)", "fileIndex": 0, "line": 590,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "T Microsoft.FSharp.Collections.ListModule::Min(Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "Min(...)", "fileIndex": 0, "line": 604,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "T Microsoft.FSharp.Collections.ListModule::MinBy(Microsoft.FSharp.Core.FSharpFunc`2<T,TResult>,Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "MinBy(...)", "fileIndex": 0, "line": 615,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<T> Microsoft.FSharp.Collections.ListModule::InsertAt(System.Int32,T,Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "InsertAt(...)", "fileIndex": 0, "line": 749,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.ListModule", "reportPath": "FSharp.Core_ListModule.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<T> Microsoft.FSharp.Collections.ListModule::InsertManyAt(System.Int32,System.Collections.Generic.IEnumerable`1<T>,Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "InsertManyAt(...)", "fileIndex": 0, "line": 767,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.MapTreeModule", "reportPath": "FSharp.Core_MapTreeModule.html", "methodName": "System.Tuple`3<TKey,TValue,Microsoft.FSharp.Collections.MapTree`2<TKey,TValue>> Microsoft.FSharp.Collections.MapTreeModule::spliceOutSuccessor(Microsoft.FSharp.Collections.MapTree`2<TKey,TValue>)", "methodShortName": "spliceOutSuccessor(...)", "fileIndex": 0, "line": 209,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.MapTreeModule", "reportPath": "FSharp.Core_MapTreeModule.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Collections.MapTree`2<TKey,TValue>> Microsoft.FSharp.Collections.MapTreeModule::collapseLHS(Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Collections.MapTree`2<TKey,TValue>>)", "methodShortName": "collapseLHS(...)", "fileIndex": 0, "line": 480,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.MapTreeModule", "reportPath": "FSharp.Core_MapTreeModule.html", "methodName": "System.Collections.Generic.KeyValuePair`2<a,b> Microsoft.FSharp.Collections.MapTreeModule::current(Microsoft.FSharp.Collections.MapTreeModule/MapIterator`2<a,b>)", "methodShortName": "current(...)", "fileIndex": 0, "line": 506,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.MapTreeModule", "reportPath": "FSharp.Core_MapTreeModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.MapTreeModule::moveNext(Microsoft.FSharp.Collections.MapTreeModule/MapIterator`2<a,b>)", "methodShortName": "moveNext(...)", "fileIndex": 0, "line": 516,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.PrivateListHelpers", "reportPath": "FSharp.Core_PrivateListHelpers.html", "methodName": "a Microsoft.FSharp.Collections.PrivateListHelpers::nth(Microsoft.FSharp.Collections.FSharpList`1<a>,System.Int32)", "methodShortName": "nth(...)", "fileIndex": 0, "line": 3697,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule::Iterate(Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.Unit>,System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "Iterate(...)", "fileIndex": 1, "line": 495,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "T Microsoft.FSharp.Collections.SeqModule::Item(System.Int32,System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "Item(...)", "fileIndex": 1, "line": 502,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<T> Microsoft.FSharp.Collections.SeqModule::TryItem(System.Int32,System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "TryItem(...)", "fileIndex": 1, "line": 509,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Void Microsoft.FSharp.Collections.SeqModule::IterateIndexed(Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.Unit>>,System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "IterateIndexed(...)", "fileIndex": 1, "line": 519,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Collections.Generic.IEnumerable`1<TResult> Microsoft.FSharp.Collections.SeqModule::Map3(Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,Microsoft.FSharp.Core.FSharpFunc`2<T3,TResult>>>,System.Collections.Generic.IEnumerable`1<T1>,System.Collections.Generic.IEnumerable`1<T2>,System.Collections.Generic.IEnumerable`1<T3>)", "methodShortName": "Map3(...)", "fileIndex": 1, "line": 617,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Collections.Generic.IEnumerable`1<System.Tuple`3<T1,T2,T3>> Microsoft.FSharp.Collections.SeqModule::Zip3(System.Collections.Generic.IEnumerable`1<T1>,System.Collections.Generic.IEnumerable`1<T2>,System.Collections.Generic.IEnumerable`1<T3>)", "methodShortName": "Zip3(...)", "fileIndex": 1, "line": 640,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "System.Collections.Generic.IEnumerable`1<T> Microsoft.FSharp.Collections.SeqModule::Take(System.Int32,System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "Take(...)", "fileIndex": 1, "line": 683,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "T Microsoft.FSharp.Collections.SeqModule::ReduceBack(Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.FSharpFunc`2<T,T>>,System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "ReduceBack(...)", "fileIndex": 1, "line": 849,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "T Microsoft.FSharp.Collections.SeqModule::Head(System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "Head(...)", "fileIndex": 1, "line": 1362,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SeqModule", "reportPath": "FSharp.Core_SeqModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<T> Microsoft.FSharp.Collections.SeqModule::TryHead(System.Collections.Generic.IEnumerable`1<T>)", "methodShortName": "TryHead(...)", "fileIndex": 1, "line": 1369,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SetTreeModule", "reportPath": "FSharp.Core_SetTreeModule.html", "methodName": "System.Tuple`2<T,Microsoft.FSharp.Collections.SetTree`1<T>> Microsoft.FSharp.Collections.SetTreeModule::spliceOutSuccessor(Microsoft.FSharp.Collections.SetTree`1<T>)", "methodShortName": "spliceOutSuccessor(...)", "fileIndex": 0, "line": 196,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SetTreeModule", "reportPath": "FSharp.Core_SetTreeModule.html", "methodName": "Microsoft.FSharp.Collections.SetTree`1<T> Microsoft.FSharp.Collections.SetTreeModule::diffAux(System.Collections.Generic.IComparer`1<T>,Microsoft.FSharp.Collections.SetTree`1<T>,Microsoft.FSharp.Collections.SetTree`1<T>)", "methodShortName": "diffAux(...)", "fileIndex": 0, "line": 297,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SetTreeModule", "reportPath": "FSharp.Core_SetTreeModule.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Collections.SetTree`1<T>> Microsoft.FSharp.Collections.SetTreeModule::collapseLHS(Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Collections.SetTree`1<T>>)", "methodShortName": "collapseLHS(...)", "fileIndex": 0, "line": 411,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SetTreeModule", "reportPath": "FSharp.Core_SetTreeModule.html", "methodName": "System.Boolean Microsoft.FSharp.Collections.SetTreeModule::moveNext(Microsoft.FSharp.Collections.SetTreeModule/SetIterator`1<a>)", "methodShortName": "moveNext(...)", "fileIndex": 0, "line": 438,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Collections.SetTreeModule", "reportPath": "FSharp.Core_SetTreeModule.html", "methodName": "System.Int32 Microsoft.FSharp.Collections.SetTreeModule::compare(System.Collections.Generic.IComparer`1<T>,Microsoft.FSharp.Collections.SetTree`1<T>,Microsoft.FSharp.Collections.SetTree`1<T>)", "methodShortName": "compare(...)", "fileIndex": 0, "line": 515,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.AsyncPrimitives", "reportPath": "FSharp.Core_AsyncPrimitives.html", "methodName": "Microsoft.FSharp.Control.AsyncReturn Microsoft.FSharp.Control.AsyncPrimitives/get_AwaitResult_NoDirectCancelOrTimeout@949::Invoke(Microsoft.FSharp.Control.AsyncActivation`1<T>)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 952,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.AsyncPrimitives", "reportPath": "FSharp.Core_AsyncPrimitives.html", "methodName": "T Microsoft.FSharp.Control.AsyncPrimitives/AsyncIAsyncResult`1::GetResult()", "methodShortName": "GetResult()", "fileIndex": 0, "line": 1216,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.FSharpAsync", "reportPath": "FSharp.Core_FSharpAsync.html", "methodName": "Microsoft.FSharp.Control.FSharpAsync`1<T> Microsoft.FSharp.Control.FSharpAsync::AwaitAndBindChildResult(System.Threading.CancellationTokenSource,Microsoft.FSharp.Control.AsyncPrimitives/ResultCell`1<Microsoft.FSharp.Control.AsyncResult`1<T>>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "AwaitAndBindChildResult(...)", "fileIndex": 0, "line": 1710,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.FSharpAsync", "reportPath": "FSharp.Core_FSharpAsync.html", "methodName": "Microsoft.FSharp.Control.AsyncReturn <StartupCode$FSharp-Core>.$Async/Parallel@1389::Invoke(Microsoft.FSharp.Control.AsyncActivation`1<T[]>)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 1392,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.FSharpAsync", "reportPath": "FSharp.Core_FSharpAsync.html", "methodName": "Microsoft.FSharp.Control.AsyncReturn <StartupCode$FSharp-Core>.$Async/Choice@1495::Invoke(Microsoft.FSharp.Control.AsyncActivation`1<Microsoft.FSharp.Core.FSharpOption`1<T>>)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 1498,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.FSharpMailboxProcessor<T>", "reportPath": "FSharp.Core_FSharpMailboxProcessor_1.html", "methodName": "Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.FSharpOption`1<TReply>> Microsoft.FSharp.Control.FSharpMailboxProcessor`1::PostAndTryAsyncReply(Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Control.FSharpAsyncReplyChannel`1<TReply>,TMsg>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "PostAndTryAsyncReply(...)", "fileIndex": 0, "line": 388,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.FSharpMailboxProcessor<T>", "reportPath": "FSharp.Core_FSharpMailboxProcessor_1.html", "methodName": "Microsoft.FSharp.Control.FSharpAsync`1<TReply> Microsoft.FSharp.Control.FSharpMailboxProcessor`1::PostAndAsyncReply(Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Control.FSharpAsyncReplyChannel`1<TReply>,TMsg>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "PostAndAsyncReply(...)", "fileIndex": 0, "line": 407,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.Mailbox<T>", "reportPath": "FSharp.Core_Mailbox_1.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<a> Microsoft.FSharp.Control.Mailbox`1::ScanInbox(Microsoft.FSharp.Core.FSharpFunc`2<TMsg,Microsoft.FSharp.Core.FSharpOption`1<a>>,System.Int32)", "methodShortName": "ScanInbox(...)", "fileIndex": 0, "line": 131,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.Mailbox<T>", "reportPath": "FSharp.Core_Mailbox_1.html", "methodName": "Microsoft.FSharp.Control.AsyncReturn <StartupCode$FSharp-Core>.$Mailbox/-ctor@77::Invoke(Microsoft.FSharp.Control.AsyncActivation`1<System.Boolean>)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 78,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.Mailbox<T>", "reportPath": "FSharp.Core_Mailbox_1.html", "methodName": "Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.FSharpOption`1<T>> <StartupCode$FSharp-Core>.$Mailbox/TryScan@233::Invoke(Microsoft.FSharp.Core.Unit)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 233,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.ObservableModule", "reportPath": "FSharp.Core_ObservableModule.html", "methodName": "System.Void Microsoft.FSharp.Control.ObservableModule/h1@133::System.IObserver<'T>.OnCompleted()", "methodShortName": "System.IObserver<'T>.OnCompleted()", "fileIndex": 0, "line": 144,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.ObservableModule", "reportPath": "FSharp.Core_ObservableModule.html", "methodName": "System.Void Microsoft.FSharp.Control.ObservableModule/h2@151::System.IObserver<'T>.OnCompleted()", "methodShortName": "System.IObserver<'T>.OnCompleted()", "fileIndex": 0, "line": 162,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.TaskBuilderExtensions.LowPriority", "reportPath": "FSharp.Core_LowPriority.html", "methodName": "System.Boolean Microsoft.FSharp.Control.TaskBuilderExtensions.LowPriority::TaskBuilderBase.BindDynamic.Static(Microsoft.FSharp.Core.CompilerServices.ResumableStateMachine`1<Microsoft.FSharp.Control.TaskStateMachineData`1<TOverall>>&,TTaskLike,Microsoft.FSharp.Core.FSharpFunc`2<TResult1,Microsoft.FSharp.Core.CompilerServices.ResumableCode`2<Microsoft.FSharp.Control.TaskStateMachineData`1<TOverall>,TResult2>>)", "methodShortName": "TaskBuilderBase.BindDynamic.Static(...)", "fileIndex": 0, "line": 268,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.TaskBuilderExtensions.LowPriority", "reportPath": "FSharp.Core_LowPriority.html", "methodName": "System.Boolean Microsoft.FSharp.Control.TaskBuilderExtensions.LowPriority/TaskBuilderBase-Bind@290::Invoke(Microsoft.FSharp.Core.CompilerServices.ResumableStateMachine`1<Microsoft.FSharp.Control.TaskStateMachineData`1<TOverall>>&)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 310,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.TaskBuilderExtensions.LowPriority", "reportPath": "FSharp.Core_LowPriority.html", "methodName": "System.Boolean Microsoft.FSharp.Control.TaskBuilderExtensions.LowPriority/TaskBuilderBase-ReturnFrom@321-2::Invoke(Microsoft.FSharp.Core.CompilerServices.ResumableStateMachine`1<Microsoft.FSharp.Control.TaskStateMachineData`1<T>>&)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 321,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.WebExtensions", "reportPath": "FSharp.Core_WebExtensions.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Control.FSharpAsync`1<System.Net.WebResponse>> Microsoft.FSharp.Control.WebExtensions/AsyncGetResponse@2005-3::Invoke(System.Exception)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 2008,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.WebExtensions", "reportPath": "FSharp.Core_WebExtensions.html", "methodName": "Microsoft.FSharp.Core.Unit Microsoft.FSharp.Control.WebExtensions/delegate'@2021::Invoke(System.Object,c)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 2021,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.ExtraTopLevelOperators", "reportPath": "FSharp.Core_ExtraTopLevelOperators.html", "methodName": "System.Collections.Generic.IEnumerator`1<System.Collections.Generic.KeyValuePair`2<TKey,T>> Microsoft.FSharp.Core.ExtraTopLevelOperators/DictImpl`3::System.Collections.Generic.IEnumerable<System.Collections.Generic.KeyValuePair<'Key, 'T>>.GetEnumerator()", "methodShortName": "System.Collections.Generic.IEnumerable<System.Collections.Generic.KeyValuePair<'Key, 'T>>.GetEnumerator()", "fileIndex": 0, "line": 120,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "T Microsoft.FSharp.Core.LanguagePrimitives::DivideByIntDynamic(T,System.Int32)", "methodShortName": "DivideByIntDynamic(...)", "fileIndex": 0, "line": 3184,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Int32 Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::checkN@990(Microsoft.FSharp.Core.LanguagePrimitives/HashCompare/GenericComparer,System.Array,System.Array,System.Int32,System.Int64[],System.Int32,System.Int64,System.Int64,System.Int64)", "methodShortName": "checkN@990(...)", "fileIndex": 0, "line": 991,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Boolean Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::GenericEqualityByteArray(System.Byte[],System.Byte[])", "methodShortName": "GenericEqualityByteArray(...)", "fileIndex": 0, "line": 1257,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Boolean Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::GenericEqualityInt32Array(System.Int32[],System.Int32[])", "methodShortName": "GenericEqualityInt32Array(...)", "fileIndex": 0, "line": 1272,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Boolean Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::f32eq@1289(System.Boolean,System.Single,System.Single)", "methodShortName": "f32eq@1289(...)", "fileIndex": 0, "line": 1289,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 6, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Boolean Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::GenericEqualitySingleArray(System.Boolean,System.Single[],System.Single[])", "methodShortName": "GenericEqualitySingleArray(...)", "fileIndex": 0, "line": 1287,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Boolean Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::feq@1306(System.Boolean,System.Double,System.Double)", "methodShortName": "feq@1306(...)", "fileIndex": 0, "line": 1306,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 6, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Boolean Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::GenericEqualityDoubleArray(System.Boolean,System.Double[],System.Double[])", "methodShortName": "GenericEqualityDoubleArray(...)", "fileIndex": 0, "line": 1303,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Boolean Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::GenericEqualityCharArray(System.Char[],System.Char[])", "methodShortName": "GenericEqualityCharArray(...)", "fileIndex": 0, "line": 1319,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Boolean Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::GenericEqualityInt64Array(System.Int64[],System.Int64[])", "methodShortName": "GenericEqualityInt64Array(...)", "fileIndex": 0, "line": 1334,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Boolean Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::precheck@1424-1(System.Array,System.Array,System.Int32,System.Int32)", "methodShortName": "precheck@1424-1(...)", "fileIndex": 0, "line": 1425,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Boolean Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::checkN@1432-1(System.Boolean,System.Collections.IEqualityComparer,System.Array,System.Array,System.Int32,System.Int64[],System.Int32,System.Int64,System.Int64,System.Int64)", "methodShortName": "checkN@1432-1(...)", "fileIndex": 0, "line": 1433,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Boolean Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::GenericEqualityObjArray(System.Boolean,System.Collections.IEqualityComparer,System.Object[],System.Object[])", "methodShortName": "GenericEqualityObjArray(...)", "fileIndex": 0, "line": 1447,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Boolean Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::FastEqualsTuple4(System.Collections.IEqualityComparer,System.Tuple`4<T1,T2,T3,T4>,System.Tuple`4<T1,T2,T3,T4>)", "methodShortName": "FastEqualsTuple4(...)", "fileIndex": 0, "line": 1914,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 6, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Int32 Microsoft.FSharp.Core.LanguagePrimitives/HashCompare::FastCompareTuple4(System.Collections.IComparer,System.Tuple`4<T1,T2,T3,T4>,System.Tuple`4<T1,T2,T3,T4>)", "methodShortName": "FastCompareTuple4(...)", "fileIndex": 0, "line": 1967,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<T> Microsoft.FSharp.Core.Operators::op_Append(Microsoft.FSharp.Collections.FSharpList`1<T>,Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "op_Append(...)", "fileIndex": 0, "line": 4172,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T Microsoft.FSharp.Core.Operators::PowInteger(T,System.Int32)", "methodShortName": "PowInteger(...)", "fileIndex": 0, "line": 6510,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T Microsoft.FSharp.Core.Operators::PowInteger$W(Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Core.Unit,T>,Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.FSharpFunc`2<T,T>>,Microsoft.FSharp.Core.FSharpFunc`2<T,Microsoft.FSharp.Core.FSharpFunc`2<T,T>>,T,System.Int32)", "methodShortName": "PowInteger$W(...)", "fileIndex": 0, "line": 6510,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "System.Int32 Microsoft.FSharp.Core.Operators/ArrayExtensions::[,]`1.GetReverseIndex(T[0...,0...],System.Int32,System.Int32)", "methodShortName": "[,]`1.GetReverseIndex(...)", "fileIndex": 0, "line": 6609,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 3, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.Operators", "reportPath": "FSharp.Core_Operators.html", "methodName": "T Microsoft.FSharp.Core.Operators/OperatorIntrinsics/BaseRangeEnumerator`1::getCurrent(Microsoft.FSharp.Core.Operators/OperatorIntrinsics/BaseRangeEnumerator`1<T>)", "methodShortName": "getCurrent(...)", "fileIndex": 0, "line": 5183,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 3, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.OptimizedClosures", "reportPath": "FSharp.Core_OptimizedClosures.html", "methodName": "Microsoft.FSharp.Core.OptimizedClosures/FSharpFunc`5<T1,T2,T3,T4,TResult> Microsoft.FSharp.Core.OptimizedClosures/FSharpFunc`5::Adapt(Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,Microsoft.FSharp.Core.FSharpFunc`2<T3,Microsoft.FSharp.Core.FSharpFunc`2<T4,TResult>>>>)", "methodShortName": "Adapt(...)", "fileIndex": 0, "line": 3316,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.OptionModule", "reportPath": "FSharp.Core_OptionModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<TResult> Microsoft.FSharp.Core.OptionModule::Map3(Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,Microsoft.FSharp.Core.FSharpFunc`2<T3,TResult>>>,Microsoft.FSharp.Core.FSharpOption`1<T1>,Microsoft.FSharp.Core.FSharpOption`1<T2>,Microsoft.FSharp.Core.FSharpOption`1<T3>)", "methodShortName": "Map3(...)", "fileIndex": 0, "line": 64,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "System.String Microsoft.FSharp.Core.PrintfImpl::valueOf@120(System.Int32)", "methodShortName": "valueOf@120(...)", "fileIndex": 0, "line": 120,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "System.Void Microsoft.FSharp.Core.PrintfImpl/SmallStringPrintfEnv4::Write(System.String)", "methodShortName": "Write(...)", "fileIndex": 0, "line": 1053,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "System.String Microsoft.FSharp.Core.PrintfImpl/FloatAndDecimal::toFormattedString(System.String,System.Object)", "methodShortName": "toFormattedString(...)", "fileIndex": 0, "line": 850,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "System.Boolean Microsoft.FSharp.Core.PrintfImpl/FloatAndDecimal::isInteger(System.Object)", "methodShortName": "isInteger(...)", "fileIndex": 0, "line": 870,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Object,System.String>> Microsoft.FSharp.Core.PrintfImpl/Integer::leftJustify(System.Boolean,Microsoft.FSharp.Core.FSharpFunc`2<System.Object,System.String>,System.String,System.Char,System.Boolean)", "methodShortName": "leftJustify(...)", "fileIndex": 0, "line": 777,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Object,System.String>> Microsoft.FSharp.Core.PrintfImpl/Integer::rightJustify(Microsoft.FSharp.Core.FSharpFunc`2<System.Object,System.String>,System.String,System.Char,System.Boolean)", "methodShortName": "rightJustify(...)", "fileIndex": 0, "line": 794,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "System.String Microsoft.FSharp.Core.PrintfImpl/GenericNumber::leftJustifyWithGFormat(System.String,System.Boolean,System.Boolean,System.Boolean,System.Int32,System.String,System.Char)", "methodShortName": "leftJustifyWithGFormat(...)", "fileIndex": 0, "line": 697,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "Microsoft.FSharp.Core.PrintfImpl/ValueConverter Microsoft.FSharp.Core.PrintfImpl/Padding::withPaddingFormatted(Microsoft.FSharp.Core.PrintfImpl/FormatSpecifier,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,System.String>,System.String,Microsoft.FSharp.Core.FSharpFunc`2<System.String,Microsoft.FSharp.Core.FSharpFunc`2<System.Object,System.String>>,Microsoft.FSharp.Core.FSharpFunc`2<System.String,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Object,System.String>>>,Microsoft.FSharp.Core.FSharpFunc`2<System.String,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Object,System.String>>>)", "methodShortName": "withPaddingFormatted(...)", "fileIndex": 0, "line": 621,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.StringModule", "reportPath": "FSharp.Core_StringModule.html", "methodName": "System.String Microsoft.FSharp.Core.StringModule::concatArray@27(System.String,System.String[])", "methodShortName": "concatArray@27(...)", "fileIndex": 0, "line": 28,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.StringModule", "reportPath": "FSharp.Core_StringModule.html", "methodName": "System.Void Microsoft.FSharp.Core.StringModule::Iterate(Microsoft.FSharp.Core.FSharpFunc`2<System.Char,Microsoft.FSharp.Core.Unit>,System.String)", "methodShortName": "Iterate(...)", "fileIndex": 0, "line": 48,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.StringModule", "reportPath": "FSharp.Core_StringModule.html", "methodName": "System.Void Microsoft.FSharp.Core.StringModule::IterateIndexed(Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Char,Microsoft.FSharp.Core.Unit>>,System.String)", "methodShortName": "IterateIndexed(...)", "fileIndex": 0, "line": 54,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.StringModule", "reportPath": "FSharp.Core_StringModule.html", "methodName": "System.String Microsoft.FSharp.Core.StringModule::MapIndexed(Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Char,System.Char>>,System.String)", "methodShortName": "MapIndexed(...)", "fileIndex": 0, "line": 74,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.StringModule", "reportPath": "FSharp.Core_StringModule.html", "methodName": "System.String Microsoft.FSharp.Core.StringModule::Initialize(System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,System.String>)", "methodShortName": "Initialize(...)", "fileIndex": 0, "line": 125,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.ValueOption", "reportPath": "FSharp.Core_ValueOption.html", "methodName": "Microsoft.FSharp.Core.FSharpValueOption`1<TResult> Microsoft.FSharp.Core.ValueOption::Map3(Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,Microsoft.FSharp.Core.FSharpFunc`2<T3,TResult>>>,Microsoft.FSharp.Core.FSharpValueOption`1<T1>,Microsoft.FSharp.Core.FSharpValueOption`1<T2>,Microsoft.FSharp.Core.FSharpValueOption`1<T3>)", "methodShortName": "Map3(...)", "fileIndex": 0, "line": 150,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Linq.QueryModule::Make$cont@599(Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>,Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>,Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`3<Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>,Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Quotations.FSharpVar,Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpExpr,System.Type,System.Type,Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Core.Unit)", "methodShortName": "Make$cont@599(...)", "fileIndex": 0, "line": 603,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Linq.QueryModule::walk@968-1(Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Core.FSharpFunc`2<a,Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Quotations.FSharpExpr>>>,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "walk@968-1(...)", "fileIndex": 0, "line": 969,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.QueryModule", "reportPath": "FSharp.Core_QueryModule.html", "methodName": "System.Tuple`2<Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Linq.RuntimeHelpers.Adapters/ConversionDescription> Microsoft.FSharp.Linq.QueryModule::TransInnerNoCheck(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "TransInnerNoCheck(...)", "fileIndex": 0, "line": 1613,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.RuntimeHelpers.Adapters", "reportPath": "FSharp.Core_Adapters.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`2<System.Reflection.ConstructorInfo,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>> Microsoft.FSharp.Linq.RuntimeHelpers.Adapters::|NewAnonymousObject|_|(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "|NewAnonymousObject|_|(...)", "fileIndex": 0, "line": 118,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.RuntimeHelpers.Adapters", "reportPath": "FSharp.Core_Adapters.html", "methodName": "System.Tuple`2<Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Linq.RuntimeHelpers.Adapters/ConversionDescription> Microsoft.FSharp.Linq.RuntimeHelpers.Adapters::ProduceMoreMutables(Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Quotations.FSharpExpr,System.Tuple`2<Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Linq.RuntimeHelpers.Adapters/ConversionDescription>>,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "ProduceMoreMutables(...)", "fileIndex": 0, "line": 274,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter", "reportPath": "FSharp.Core_LeafExpressionConverter.html", "methodName": "System.Boolean Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter::equivHeadTypes(System.Type,System.Type)", "methodShortName": "equivHeadTypes(...)", "fileIndex": 0, "line": 45,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.Array", "reportPath": "FSharp.Core_Array.html", "methodName": "System.Void Microsoft.FSharp.Primitives.Basics.Array::unstableSortInPlaceBy(Microsoft.FSharp.Core.FSharpFunc`2<T,TKey>,T[])", "methodShortName": "unstableSortInPlaceBy(...)", "fileIndex": 0, "line": 1079,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.Array", "reportPath": "FSharp.Core_Array.html", "methodName": "System.Void Microsoft.FSharp.Primitives.Basics.Array::stableSortInPlaceBy(Microsoft.FSharp.Core.FSharpFunc`2<T,TKey>,T[])", "methodShortName": "stableSortInPlaceBy(...)", "fileIndex": 0, "line": 1122,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "System.Void Microsoft.FSharp.Primitives.Basics.List::map2ToFreshConsTail(Microsoft.FSharp.Collections.FSharpList`1<a>,Microsoft.FSharp.Core.OptimizedClosures/FSharpFunc`3<b,c,a>,Microsoft.FSharp.Collections.FSharpList`1<b>,Microsoft.FSharp.Collections.FSharpList`1<c>)", "methodShortName": "map2ToFreshConsTail(...)", "fileIndex": 0, "line": 272,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<TResult> Microsoft.FSharp.Primitives.Basics.List::map2(Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,TResult>>,Microsoft.FSharp.Collections.FSharpList`1<T1>,Microsoft.FSharp.Collections.FSharpList`1<T2>)", "methodShortName": "map2(...)", "fileIndex": 0, "line": 282,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "System.Void Microsoft.FSharp.Primitives.Basics.List::mapi2ToFreshConsTail(System.Int32,Microsoft.FSharp.Collections.FSharpList`1<a>,Microsoft.FSharp.Core.OptimizedClosures/FSharpFunc`4<System.Int32,b,c,a>,Microsoft.FSharp.Collections.FSharpList`1<b>,Microsoft.FSharp.Collections.FSharpList`1<c>)", "methodShortName": "mapi2ToFreshConsTail(...)", "fileIndex": 0, "line": 316,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<TResult> Microsoft.FSharp.Primitives.Basics.List::mapi2(Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<T1,Microsoft.FSharp.Core.FSharpFunc`2<T2,TResult>>>,Microsoft.FSharp.Collections.FSharpList`1<T1>,Microsoft.FSharp.Collections.FSharpList`1<T2>)", "methodShortName": "mapi2(...)", "fileIndex": 0, "line": 326,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<T> Microsoft.FSharp.Primitives.Basics.List::take(System.Int32,Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "take(...)", "fileIndex": 0, "line": 582,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<T> Microsoft.FSharp.Primitives.Basics.List::truncate(System.Int32,Microsoft.FSharp.Collections.FSharpList`1<T>)", "methodShortName": "truncate(...)", "fileIndex": 0, "line": 750,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "System.Void Microsoft.FSharp.Primitives.Basics.List::zipToFreshConsTail(Microsoft.FSharp.Collections.FSharpList`1<System.Tuple`2<a,b>>,Microsoft.FSharp.Collections.FSharpList`1<a>,Microsoft.FSharp.Collections.FSharpList`1<b>)", "methodShortName": "zipToFreshConsTail(...)", "fileIndex": 0, "line": 909,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Primitives.Basics.List", "reportPath": "FSharp.Core_List.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<System.Tuple`2<T1,T2>> Microsoft.FSharp.Primitives.Basics.List::zip(Microsoft.FSharp.Collections.FSharpList`1<T1>,Microsoft.FSharp.Collections.FSharpList`1<T2>)", "methodShortName": "zip(...)", "fileIndex": 0, "line": 921,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.DerivedPatternsModule", "reportPath": "FSharp.Core_DerivedPatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`2<Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>> Microsoft.FSharp.Quotations.DerivedPatternsModule::|TupledApplication|_|(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "|TupledApplication|_|(...)", "fileIndex": 0, "line": 2173,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.DerivedPatternsModule", "reportPath": "FSharp.Core_DerivedPatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`2<Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Quotations.DerivedPatternsModule::AndAlsoPattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "AndAlsoPattern(...)", "fileIndex": 0, "line": 2188,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.DerivedPatternsModule", "reportPath": "FSharp.Core_DerivedPatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`2<Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Quotations.DerivedPatternsModule::OrElsePattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "OrElsePattern(...)", "fileIndex": 0, "line": 2194,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.ExprShapeModule", "reportPath": "FSharp.Core_ExprShapeModule.html", "methodName": "Microsoft.FSharp.Core.FSharpChoice`3<Microsoft.FSharp.Quotations.FSharpVar,System.Tuple`2<Microsoft.FSharp.Quotations.FSharpVar,Microsoft.FSharp.Quotations.FSharpExpr>,System.Tuple`2<System.Object,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>>> Microsoft.FSharp.Quotations.ExprShapeModule::loop@2304-48(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "loop@2304-48(...)", "fileIndex": 0, "line": 2307,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.FSharpVar", "reportPath": "FSharp.Core_FSharpVar.html", "methodName": "Microsoft.FSharp.Quotations.FSharpVar Microsoft.FSharp.Quotations.FSharpVar::Global(System.String,System.Type)", "methodShortName": "Global(...)", "fileIndex": 0, "line": 102,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`2<Microsoft.FSharp.Quotations.ExprConstInfo,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Quotations.PatternsModule::|Comb1|_|(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "|Comb1|_|(...)", "fileIndex": 0, "line": 408,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Quotations.PatternsModule::NewStructTuplePattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "NewStructTuplePattern(...)", "fileIndex": 0, "line": 439,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Type> Microsoft.FSharp.Quotations.PatternsModule::DefaultValuePattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "DefaultValuePattern(...)", "fileIndex": 0, "line": 442,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`3<System.Object,System.Type,System.String>> Microsoft.FSharp.Quotations.PatternsModule::ValueWithNamePattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "ValueWithNamePattern(...)", "fileIndex": 0, "line": 486,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`4<Microsoft.FSharp.Quotations.FSharpVar,Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Quotations.PatternsModule::ForIntegerRangeLoopPattern(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "ForIntegerRangeLoopPattern(...)", "fileIndex": 0, "line": 510,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpVar>,Microsoft.FSharp.Quotations.FSharpExpr>> Microsoft.FSharp.Quotations.PatternsModule::|NLambdas|_|(System.Int32,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "|NLambdas|_|(...)", "fileIndex": 0, "line": 610,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "System.Void Microsoft.FSharp.Quotations.PatternsModule::checkAppliedLambda(Microsoft.FSharp.Quotations.FSharpExpr,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "checkAppliedLambda(...)", "fileIndex": 0, "line": 766,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Quotations.PatternsModule::mkStaticPropGet(System.Reflection.PropertyInfo,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>)", "methodShortName": "mkStaticPropGet(...)", "fileIndex": 0, "line": 909,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Quotations.PatternsModule::mkInstancePropGet(Microsoft.FSharp.Quotations.FSharpExpr,System.Reflection.PropertyInfo,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>)", "methodShortName": "mkInstancePropGet(...)", "fileIndex": 0, "line": 917,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Quotations.PatternsModule::mkStaticPropSet(System.Reflection.PropertyInfo,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "mkStaticPropSet(...)", "fileIndex": 0, "line": 927,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Quotations.FSharpExpr Microsoft.FSharp.Quotations.PatternsModule::mkInstancePropSet(Microsoft.FSharp.Quotations.FSharpExpr,System.Reflection.PropertyInfo,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpExpr>,Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "mkInstancePropSet(...)", "fileIndex": 0, "line": 935,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "System.Reflection.MethodInfo Microsoft.FSharp.Quotations.PatternsModule::bindMethodBySearch(System.Type,System.String,System.Int32,Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,System.Type>,System.Type>>,Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,System.Type>,System.Type>)", "methodShortName": "bindMethodBySearch(...)", "fileIndex": 0, "line": 1043,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "System.Reflection.MethodInfo Microsoft.FSharp.Quotations.PatternsModule::bindModuleFunctionWithCallSiteArgs(System.Type,System.String,Microsoft.FSharp.Collections.FSharpList`1<System.Type>,Microsoft.FSharp.Collections.FSharpList`1<System.Type>)", "methodShortName": "bindModuleFunctionWithCallSiteArgs(...)", "fileIndex": 0, "line": 1098,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "System.Type Microsoft.FSharp.Quotations.PatternsModule::decodeFunTy(Microsoft.FSharp.Collections.FSharpList`1<System.Type>)", "methodShortName": "decodeFunTy(...)", "fileIndex": 0, "line": 1390,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "System.Type Microsoft.FSharp.Quotations.PatternsModule::decodeArrayTy(System.Int32,Microsoft.FSharp.Collections.FSharpList`1<System.Type>)", "methodShortName": "decodeArrayTy(...)", "fileIndex": 0, "line": 1395,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "System.Reflection.Assembly Microsoft.FSharp.Quotations.PatternsModule::decodeAssemblyRef(Microsoft.FSharp.Quotations.PatternsModule/SimpleUnpickle/InputState,System.String)", "methodShortName": "decodeAssemblyRef(...)", "fileIndex": 0, "line": 1415,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Core.FSharpFunc`2<System.Int32,System.Type>,System.Type> Microsoft.FSharp.Quotations.PatternsModule::u_dtype(Microsoft.FSharp.Quotations.PatternsModule/SimpleUnpickle/InputState)", "methodShortName": "u_dtype(...)", "fileIndex": 0, "line": 1448,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 3, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "System.Boolean Microsoft.FSharp.Quotations.PatternsModule/select@1054::Invoke(System.Reflection.MethodInfo)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 1054,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "System.Boolean Microsoft.FSharp.Quotations.PatternsModule/candidates@1112::Invoke(System.Reflection.MethodInfo)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 1113,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<a> Microsoft.FSharp.Quotations.PatternsModule/SimpleUnpickle::u_list_aux(Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Quotations.PatternsModule/SimpleUnpickle/InputState,a>,Microsoft.FSharp.Collections.FSharpList`1<a>,Microsoft.FSharp.Quotations.PatternsModule/SimpleUnpickle/InputState)", "methodShortName": "u_list_aux(...)", "fileIndex": 0, "line": 1361,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Quotations.PatternsModule", "reportPath": "FSharp.Core_PatternsModule.html", "methodName": "Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`2<Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Quotations.FSharpVar>,Microsoft.FSharp.Quotations.FSharpExpr>> <StartupCode$FSharp-Core>.$Quotations/|NLambdas|_|@265-1::Invoke(Microsoft.FSharp.Quotations.FSharpExpr)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 266,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.FSharpValue", "reportPath": "FSharp.Core_FSharpValue.html", "methodName": "System.Object Microsoft.FSharp.Reflection.FSharpValue::GetRecordField(System.Object,System.Reflection.PropertyInfo)", "methodShortName": "GetRecordField(...)", "fileIndex": 0, "line": 999,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.FSharpValue", "reportPath": "FSharp.Core_FSharpValue.html", "methodName": "System.Object[] Microsoft.FSharp.Reflection.FSharpValue::GetRecordFields(System.Object,Microsoft.FSharp.Core.FSharpOption`1<System.Reflection.BindingFlags>)", "methodShortName": "GetRecordFields(...)", "fileIndex": 0, "line": 1005,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Boolean Microsoft.FSharp.Reflection.Impl::equivHeadTypes(System.Type,System.Type)", "methodShortName": "equivHeadTypes(...)", "fileIndex": 0, "line": 41,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Linq.Expressions.NewExpression Microsoft.FSharp.Reflection.Impl::constituentTuple@162(System.Int32,Microsoft.FSharp.Core.FSharpFunc`2<System.Type,a>,System.Type,System.Linq.Expressions.Expression,System.Int32)", "methodShortName": "constituentTuple@162(...)", "fileIndex": 0, "line": 163,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Tuple`2<System.Int32,System.String>[] Microsoft.FSharp.Reflection.Impl::getUnionTypeTagNameMap(System.Type,System.Reflection.BindingFlags)", "methodShortName": "getUnionTypeTagNameMap(...)", "fileIndex": 0, "line": 332,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "Microsoft.FSharp.Core.FSharpFunc`2<System.Object,System.Int32> Microsoft.FSharp.Reflection.Impl::getUnionTagReader(System.Type,System.Reflection.BindingFlags)", "methodShortName": "getUnionTagReader(...)", "fileIndex": 0, "line": 447,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "Microsoft.FSharp.Core.FSharpFunc`2<System.Object,System.Int32> Microsoft.FSharp.Reflection.Impl::getUnionTagReaderCompiled(System.Type,System.Reflection.BindingFlags)", "methodShortName": "getUnionTagReaderCompiled(...)", "fileIndex": 0, "line": 461,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Void Microsoft.FSharp.Reflection.Impl::checkUnionType(System.Type,System.Reflection.BindingFlags)", "methodShortName": "checkUnionType(...)", "fileIndex": 0, "line": 504,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Reflection.PropertyInfo Microsoft.FSharp.Reflection.Impl::get@736-3(System.Type,System.Int32)", "methodShortName": "get@736-3(...)", "fileIndex": 0, "line": 737,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Boolean Microsoft.FSharp.Reflection.Impl::isRecordType(System.Type,System.Reflection.BindingFlags)", "methodShortName": "isRecordType(...)", "fileIndex": 0, "line": 771,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Reflection.ConstructorInfo Microsoft.FSharp.Reflection.Impl::getRecordConstructorMethod(System.Type,System.Reflection.BindingFlags)", "methodShortName": "getRecordConstructorMethod(...)", "fileIndex": 0, "line": 795,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Boolean Microsoft.FSharp.Reflection.Impl::isExceptionRepr(System.Type,System.Reflection.BindingFlags)", "methodShortName": "isExceptionRepr(...)", "fileIndex": 0, "line": 815,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Type Microsoft.FSharp.Reflection.Impl::getTypeOfReprType(System.Type,System.Reflection.BindingFlags)", "methodShortName": "getTypeOfReprType(...)", "fileIndex": 0, "line": 826,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.Void Microsoft.FSharp.Reflection.Impl::checkRecordType(System.String,System.Type,System.Reflection.BindingFlags)", "methodShortName": "checkRecordType(...)", "fileIndex": 0, "line": 845,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.String Microsoft.FSharp.Reflection.Impl/getUnionTagConverter@386::Invoke(System.Int32)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 386,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Reflection.Impl", "reportPath": "FSharp.Core_Impl.html", "methodName": "System.String Microsoft.FSharp.Reflection.Impl/getUnionTagConverter@387-1::Invoke(System.Int32)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 387,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "reportPath": "FSharp.Core_Display.html", "methodName": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display/Breaks Microsoft.FSharp.Text.StructuredPrintfImpl.Display::pushBreak(System.Int32,Microsoft.FSharp.Text.StructuredPrintfImpl.Display/Breaks)", "methodShortName": "pushBreak(...)", "fileIndex": 0, "line": 569,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "reportPath": "FSharp.Core_Display.html", "methodName": "System.Boolean Microsoft.FSharp.Text.StructuredPrintfImpl.Display::check@837-6(System.String,System.Int32)", "methodShortName": "check@837-6(...)", "fileIndex": 0, "line": 837,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 2, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "reportPath": "FSharp.Core_Display.html", "methodName": "Microsoft.FSharp.Text.StructuredPrintfImpl.Layout Microsoft.FSharp.Text.StructuredPrintfImpl.Display/ObjectGraphFormatter::mapSetValueL(System.Int32,Microsoft.FSharp.Text.StructuredPrintfImpl.Display/Precedence,System.Type,System.Object)", "methodShortName": "mapSetValueL(...)", "fileIndex": 0, "line": 1133,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "reportPath": "FSharp.Core_Display.html", "methodName": "Microsoft.FSharp.Text.StructuredPrintfImpl.Layout Microsoft.FSharp.Text.StructuredPrintfImpl.Display/ObjectGraphFormatter::sequenceValueL(Microsoft.FSharp.Text.StructuredPrintfImpl.Display/ShowMode,System.Int32,Microsoft.FSharp.Text.StructuredPrintfImpl.Display/Precedence,System.Collections.IEnumerable)", "methodShortName": "sequenceValueL(...)", "fileIndex": 0, "line": 1154,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.Display", "reportPath": "FSharp.Core_Display.html", "methodName": "System.Boolean Microsoft.FSharp.Text.StructuredPrintfImpl.Display/propsAndFields@1186::Invoke(System.Reflection.MemberInfo)", "methodShortName": "Invoke(...)", "fileIndex": 0, "line": 1189,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 6, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.LayoutModule", "reportPath": "FSharp.Core_LayoutModule.html", "methodName": "System.Boolean Microsoft.FSharp.Text.StructuredPrintfImpl.LayoutModule::isEmptyL(Microsoft.FSharp.Text.StructuredPrintfImpl.Layout)", "methodShortName": "isEmptyL(...)", "fileIndex": 0, "line": 270,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 0, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.LayoutModule", "reportPath": "FSharp.Core_LayoutModule.html", "methodName": "Microsoft.FSharp.Collections.FSharpList`1<Microsoft.FSharp.Text.StructuredPrintfImpl.Layout> Microsoft.FSharp.Text.StructuredPrintfImpl.LayoutModule::consume@368(Microsoft.FSharp.Core.FSharpFunc`2<a,Microsoft.FSharp.Text.StructuredPrintfImpl.Layout>,Microsoft.FSharp.Core.FSharpFunc`2<z,Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`2<a,z>>>,Microsoft.FSharp.Core.FSharpFunc`2<z,System.Boolean>,System.Int32,z)", "methodShortName": "consume@368(...)", "fileIndex": 0, "line": 369,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.ReflectUtils", "reportPath": "FSharp.Core_ReflectUtils.html", "methodName": "System.Boolean Microsoft.FSharp.Text.StructuredPrintfImpl.ReflectUtils::equivHeadTypes(System.Type,System.Type)", "methodShortName": "equivHeadTypes(...)", "fileIndex": 0, "line": 429,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 8, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Text.StructuredPrintfImpl.ReflectUtils", "reportPath": "FSharp.Core_ReflectUtils.html", "methodName": "System.Boolean Microsoft.FSharp.Text.StructuredPrintfImpl.ReflectUtils::isListType(System.Type)", "methodShortName": "isListType(...)", "fileIndex": 0, "line": 444,
    "metrics": [
      { "value": 4, "exceeded": false },
      { "value": 4, "exceeded": false },
      { "value": 20, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Collections.Generic.IComparer`1<T> Microsoft.FSharp.Core.LanguagePrimitives::.cctor$cont@2201-4(System.Type,Microsoft.FSharp.Core.Unit)", "methodShortName": ".cctor$cont@2201-4(...)", "fileIndex": 0, "line": 2211,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 128, "exceeded": false },
      { "value": 17.71, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.PrintfImpl", "reportPath": "FSharp.Core_PrintfImpl.html", "methodName": "Microsoft.FSharp.Core.PrintfImpl/FormatFlags Microsoft.FSharp.Core.PrintfImpl/FormatString::parseFlags(System.String,System.Int32&)", "methodShortName": "parseFlags(...)", "fileIndex": 0, "line": 157,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 16, "exceeded": false },
      { "value": 16.14, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Core.LanguagePrimitives", "reportPath": "FSharp.Core_LanguagePrimitives.html", "methodName": "System.Collections.Generic.IComparer`1<T> Microsoft.FSharp.Core.LanguagePrimitives::.cctor$cont@2201-3(System.Type,Microsoft.FSharp.Core.Unit)", "methodShortName": ".cctor$cont@2201-3(...)", "fileIndex": 0, "line": 2204,
    "metrics": [
      { "value": 8, "exceeded": false },
      { "value": 128, "exceeded": false },
      { "value": 16, "exceeded": true },
    ]},
  {
    "assembly": "FSharp.Core", "class": "Microsoft.FSharp.Control.AsyncPrimitives", "reportPath": "FSharp.Core_AsyncPrimitives.html", "methodName": "a Microsoft.FSharp.Control.AsyncPrimitives::QueueAsyncAndWaitForResultSynchronously(System.Threading.CancellationToken,Microsoft.FSharp.Control.FSharpAsync`1<a>,Microsoft.FSharp.Core.FSharpOption`1<System.Int32>)", "methodShortName": "QueueAsyncAndWaitForResultSynchronously(...)", "fileIndex": 0, "line": 1012,
    "metrics": [
      { "value": 7, "exceeded": false },
      { "value": 32, "exceeded": false },
      { "value": 15.72, "exceeded": true },
    ]},
];

var branchCoverageAvailable = true;


var translations = {
'top': 'Top:',
'all': 'All',
'assembly': 'Assembly',
'class': 'Class',
'method': 'Method',
'lineCoverage': 'LineCoverage',
'noGrouping': 'No grouping',
'byAssembly': 'By assembly',
'byNamespace': 'By namespace, Level:',
'all': 'All',
'collapseAll': 'Collapse all',
'expandAll': 'Expand all',
'grouping': 'Grouping:',
'filter': 'Filter:',
'name': 'Name',
'covered': 'Covered',
'uncovered': 'Uncovered',
'coverable': 'Coverable',
'total': 'Total',
'coverage': 'Line coverage',
'branchCoverage': 'Branch coverage',
'history': 'Coverage History',
'compareHistory': 'Compare with:',
'date': 'Date',
'allChanges': 'All changes',
'lineCoverageIncreaseOnly': 'Line coverage: Increase only',
'lineCoverageDecreaseOnly': 'Line coverage: Decrease only',
'branchCoverageIncreaseOnly': 'Branch coverage: Increase only',
'branchCoverageDecreaseOnly': 'Branch coverage: Decrease only'
};


(()=>{"use strict";var e,_={},p={};function n(e){var a=p[e];if(void 0!==a)return a.exports;var r=p[e]={exports:{}};return _[e](r,r.exports,n),r.exports}n.m=_,e=[],n.O=(a,r,u,l)=>{if(!r){var c=1/0;for(f=0;f<e.length;f++){for(var[r,u,l]=e[f],v=!0,t=0;t<r.length;t++)(!1&l||c>=l)&&Object.keys(n.O).every(d=>n.O[d](r[t]))?r.splice(t--,1):(v=!1,l<c&&(c=l));if(v){e.splice(f--,1);var o=u();void 0!==o&&(a=o)}}return a}l=l||0;for(var f=e.length;f>0&&e[f-1][2]>l;f--)e[f]=e[f-1];e[f]=[r,u,l]},n.n=e=>{var a=e&&e.__esModule?()=>e.default:()=>e;return n.d(a,{a}),a},n.d=(e,a)=>{for(var r in a)n.o(a,r)&&!n.o(e,r)&&Object.defineProperty(e,r,{enumerable:!0,get:a[r]})},n.o=(e,a)=>Object.prototype.hasOwnProperty.call(e,a),(()=>{var e={666:0};n.O.j=u=>0===e[u];var a=(u,l)=>{var t,o,[f,c,v]=l,s=0;for(t in c)n.o(c,t)&&(n.m[t]=c[t]);if(v)var b=v(n);for(u&&u(l);s<f.length;s++)n.o(e,o=f[s])&&e[o]&&e[o][0](),e[f[s]]=0;return n.O(b)},r=self.webpackChunkcoverage_app=self.webpackChunkcoverage_app||[];r.forEach(a.bind(null,0)),r.push=a.bind(null,r.push.bind(r))})()})();
"use strict";(self.webpackChunkcoverage_app=self.webpackChunkcoverage_app||[]).push([[429],{273:()=>{!function(e){const n=e.performance;function i(I){n&&n.mark&&n.mark(I)}function r(I,p){n&&n.measure&&n.measure(I,p)}i("Zone");const c=e.__Zone_symbol_prefix||"__zone_symbol__";function u(I){return c+I}const f=!0===e[u("forceDuplicateZoneCheck")];if(e.Zone){if(f||"function"!=typeof e.Zone.__symbol__)throw new Error("Zone already loaded.");return e.Zone}let _=(()=>{class I{constructor(t,o){this._parent=t,this._name=o?o.name||"unnamed":"<root>",this._properties=o&&o.properties||{},this._zoneDelegate=new T(this,this._parent&&this._parent._zoneDelegate,o)}static assertZonePatched(){if(e.Promise!==J.ZoneAwarePromise)throw new Error("Zone.js has detected that ZoneAwarePromise `(window|global).Promise` has been overwritten.\nMost likely cause is that a Promise polyfill has been loaded after Zone.js (Polyfilling Promise api is not necessary when zone.js is loaded. If you must load one, do so before loading zone.js.)")}static get root(){let t=I.current;for(;t.parent;)t=t.parent;return t}static get current(){return G.zone}static get currentTask(){return te}static __load_patch(t,o,g=!1){if(J.hasOwnProperty(t)){if(!g&&f)throw Error("Already loaded patch: "+t)}else if(!e["__Zone_disable_"+t]){const P="Zone:"+t;i(P),J[t]=o(e,I,le),r(P,P)}}get parent(){return this._parent}get name(){return this._name}get(t){const o=this.getZoneWith(t);if(o)return o._properties[t]}getZoneWith(t){let o=this;for(;o;){if(o._properties.hasOwnProperty(t))return o;o=o._parent}return null}fork(t){if(!t)throw new Error("ZoneSpec required!");return this._zoneDelegate.fork(this,t)}wrap(t,o){if("function"!=typeof t)throw new Error("Expecting function got: "+t);const g=this._zoneDelegate.intercept(this,t,o),P=this;return function(){return P.runGuarded(g,this,arguments,o)}}run(t,o,g,P){G={parent:G,zone:this};try{return this._zoneDelegate.invoke(this,t,o,g,P)}finally{G=G.parent}}runGuarded(t,o=null,g,P){G={parent:G,zone:this};try{try{return this._zoneDelegate.invoke(this,t,o,g,P)}catch(K){if(this._zoneDelegate.handleError(this,K))throw K}}finally{G=G.parent}}runTask(t,o,g){if(t.zone!=this)throw new Error("A task can only be run in the zone of creation! (Creation: "+(t.zone||z).name+"; Execution: "+this.name+")");if(t.state===j&&(t.type===R||t.type===M))return;const P=t.state!=X;P&&t._transitionTo(X,O),t.runCount++;const K=te;te=t,G={parent:G,zone:this};try{t.type==M&&t.data&&!t.data.isPeriodic&&(t.cancelFn=void 0);try{return this._zoneDelegate.invokeTask(this,t,o,g)}catch(l){if(this._zoneDelegate.handleError(this,l))throw l}}finally{t.state!==j&&t.state!==Y&&(t.type==R||t.data&&t.data.isPeriodic?P&&t._transitionTo(O,X):(t.runCount=0,this._updateTaskCount(t,-1),P&&t._transitionTo(j,X,j))),G=G.parent,te=K}}scheduleTask(t){if(t.zone&&t.zone!==this){let g=this;for(;g;){if(g===t.zone)throw Error(`can not reschedule task to ${this.name} which is descendants of the original zone ${t.zone.name}`);g=g.parent}}t._transitionTo(q,j);const o=[];t._zoneDelegates=o,t._zone=this;try{t=this._zoneDelegate.scheduleTask(this,t)}catch(g){throw t._transitionTo(Y,q,j),this._zoneDelegate.handleError(this,g),g}return t._zoneDelegates===o&&this._updateTaskCount(t,1),t.state==q&&t._transitionTo(O,q),t}scheduleMicroTask(t,o,g,P){return this.scheduleTask(new m(v,t,o,g,P,void 0))}scheduleMacroTask(t,o,g,P,K){return this.scheduleTask(new m(M,t,o,g,P,K))}scheduleEventTask(t,o,g,P,K){return this.scheduleTask(new m(R,t,o,g,P,K))}cancelTask(t){if(t.zone!=this)throw new Error("A task can only be cancelled in the zone of creation! (Creation: "+(t.zone||z).name+"; Execution: "+this.name+")");t._transitionTo(A,O,X);try{this._zoneDelegate.cancelTask(this,t)}catch(o){throw t._transitionTo(Y,A),this._zoneDelegate.handleError(this,o),o}return this._updateTaskCount(t,-1),t._transitionTo(j,A),t.runCount=0,t}_updateTaskCount(t,o){const g=t._zoneDelegates;-1==o&&(t._zoneDelegates=null);for(let P=0;P<g.length;P++)g[P]._updateTaskCount(t.type,o)}}return I.__symbol__=u,I})();const y={name:"",onHasTask:(I,p,t,o)=>I.hasTask(t,o),onScheduleTask:(I,p,t,o)=>I.scheduleTask(t,o),onInvokeTask:(I,p,t,o,g,P)=>I.invokeTask(t,o,g,P),onCancelTask:(I,p,t,o)=>I.cancelTask(t,o)};class T{constructor(p,t,o){this._taskCounts={microTask:0,macroTask:0,eventTask:0},this.zone=p,this._parentDelegate=t,this._forkZS=o&&(o&&o.onFork?o:t._forkZS),this._forkDlgt=o&&(o.onFork?t:t._forkDlgt),this._forkCurrZone=o&&(o.onFork?this.zone:t._forkCurrZone),this._interceptZS=o&&(o.onIntercept?o:t._interceptZS),this._interceptDlgt=o&&(o.onIntercept?t:t._interceptDlgt),this._interceptCurrZone=o&&(o.onIntercept?this.zone:t._interceptCurrZone),this._invokeZS=o&&(o.onInvoke?o:t._invokeZS),this._invokeDlgt=o&&(o.onInvoke?t:t._invokeDlgt),this._invokeCurrZone=o&&(o.onInvoke?this.zone:t._invokeCurrZone),this._handleErrorZS=o&&(o.onHandleError?o:t._handleErrorZS),this._handleErrorDlgt=o&&(o.onHandleError?t:t._handleErrorDlgt),this._handleErrorCurrZone=o&&(o.onHandleError?this.zone:t._handleErrorCurrZone),this._scheduleTaskZS=o&&(o.onScheduleTask?o:t._scheduleTaskZS),this._scheduleTaskDlgt=o&&(o.onScheduleTask?t:t._scheduleTaskDlgt),this._scheduleTaskCurrZone=o&&(o.onScheduleTask?this.zone:t._scheduleTaskCurrZone),this._invokeTaskZS=o&&(o.onInvokeTask?o:t._invokeTaskZS),this._invokeTaskDlgt=o&&(o.onInvokeTask?t:t._invokeTaskDlgt),this._invokeTaskCurrZone=o&&(o.onInvokeTask?this.zone:t._invokeTaskCurrZone),this._cancelTaskZS=o&&(o.onCancelTask?o:t._cancelTaskZS),this._cancelTaskDlgt=o&&(o.onCancelTask?t:t._cancelTaskDlgt),this._cancelTaskCurrZone=o&&(o.onCancelTask?this.zone:t._cancelTaskCurrZone),this._hasTaskZS=null,this._hasTaskDlgt=null,this._hasTaskDlgtOwner=null,this._hasTaskCurrZone=null;const g=o&&o.onHasTask;(g||t&&t._hasTaskZS)&&(this._hasTaskZS=g?o:y,this._hasTaskDlgt=t,this._hasTaskDlgtOwner=this,this._hasTaskCurrZone=p,o.onScheduleTask||(this._scheduleTaskZS=y,this._scheduleTaskDlgt=t,this._scheduleTaskCurrZone=this.zone),o.onInvokeTask||(this._invokeTaskZS=y,this._invokeTaskDlgt=t,this._invokeTaskCurrZone=this.zone),o.onCancelTask||(this._cancelTaskZS=y,this._cancelTaskDlgt=t,this._cancelTaskCurrZone=this.zone))}fork(p,t){return this._forkZS?this._forkZS.onFork(this._forkDlgt,this.zone,p,t):new _(p,t)}intercept(p,t,o){return this._interceptZS?this._interceptZS.onIntercept(this._interceptDlgt,this._interceptCurrZone,p,t,o):t}invoke(p,t,o,g,P){return this._invokeZS?this._invokeZS.onInvoke(this._invokeDlgt,this._invokeCurrZone,p,t,o,g,P):t.apply(o,g)}handleError(p,t){return!this._handleErrorZS||this._handleErrorZS.onHandleError(this._handleErrorDlgt,this._handleErrorCurrZone,p,t)}scheduleTask(p,t){let o=t;if(this._scheduleTaskZS)this._hasTaskZS&&o._zoneDelegates.push(this._hasTaskDlgtOwner),o=this._scheduleTaskZS.onScheduleTask(this._scheduleTaskDlgt,this._scheduleTaskCurrZone,p,t),o||(o=t);else if(t.scheduleFn)t.scheduleFn(t);else{if(t.type!=v)throw new Error("Task is missing scheduleFn.");d(t)}return o}invokeTask(p,t,o,g){return this._invokeTaskZS?this._invokeTaskZS.onInvokeTask(this._invokeTaskDlgt,this._invokeTaskCurrZone,p,t,o,g):t.callback.apply(o,g)}cancelTask(p,t){let o;if(this._cancelTaskZS)o=this._cancelTaskZS.onCancelTask(this._cancelTaskDlgt,this._cancelTaskCurrZone,p,t);else{if(!t.cancelFn)throw Error("Task is not cancelable");o=t.cancelFn(t)}return o}hasTask(p,t){try{this._hasTaskZS&&this._hasTaskZS.onHasTask(this._hasTaskDlgt,this._hasTaskCurrZone,p,t)}catch(o){this.handleError(p,o)}}_updateTaskCount(p,t){const o=this._taskCounts,g=o[p],P=o[p]=g+t;if(P<0)throw new Error("More tasks executed then were scheduled.");0!=g&&0!=P||this.hasTask(this.zone,{microTask:o.microTask>0,macroTask:o.macroTask>0,eventTask:o.eventTask>0,change:p})}}class m{constructor(p,t,o,g,P,K){if(this._zone=null,this.runCount=0,this._zoneDelegates=null,this._state="notScheduled",this.type=p,this.source=t,this.data=g,this.scheduleFn=P,this.cancelFn=K,!o)throw new Error("callback is not defined");this.callback=o;const l=this;this.invoke=p===R&&g&&g.useG?m.invokeTask:function(){return m.invokeTask.call(e,l,this,arguments)}}static invokeTask(p,t,o){p||(p=this),re++;try{return p.runCount++,p.zone.runTask(p,t,o)}finally{1==re&&L(),re--}}get zone(){return this._zone}get state(){return this._state}cancelScheduleRequest(){this._transitionTo(j,q)}_transitionTo(p,t,o){if(this._state!==t&&this._state!==o)throw new Error(`${this.type} '${this.source}': can not transition to '${p}', expecting state '${t}'${o?" or '"+o+"'":""}, was '${this._state}'.`);this._state=p,p==j&&(this._zoneDelegates=null)}toString(){return this.data&&void 0!==this.data.handleId?this.data.handleId.toString():Object.prototype.toString.call(this)}toJSON(){return{type:this.type,state:this.state,source:this.source,zone:this.zone.name,runCount:this.runCount}}}const S=u("setTimeout"),D=u("Promise"),Z=u("then");let E,B=[],V=!1;function d(I){if(0===re&&0===B.length)if(E||e[D]&&(E=e[D].resolve(0)),E){let p=E[Z];p||(p=E.then),p.call(E,L)}else e[S](L,0);I&&B.push(I)}function L(){if(!V){for(V=!0;B.length;){const I=B;B=[];for(let p=0;p<I.length;p++){const t=I[p];try{t.zone.runTask(t,null,null)}catch(o){le.onUnhandledError(o)}}}le.microtaskDrainDone(),V=!1}}const z={name:"NO ZONE"},j="notScheduled",q="scheduling",O="scheduled",X="running",A="canceling",Y="unknown",v="microTask",M="macroTask",R="eventTask",J={},le={symbol:u,currentZoneFrame:()=>G,onUnhandledError:F,microtaskDrainDone:F,scheduleMicroTask:d,showUncaughtError:()=>!_[u("ignoreConsoleErrorUncaughtError")],patchEventTarget:()=>[],patchOnProperties:F,patchMethod:()=>F,bindArguments:()=>[],patchThen:()=>F,patchMacroTask:()=>F,patchEventPrototype:()=>F,isIEOrEdge:()=>!1,getGlobalObjects:()=>{},ObjectDefineProperty:()=>F,ObjectGetOwnPropertyDescriptor:()=>{},ObjectCreate:()=>{},ArraySlice:()=>[],patchClass:()=>F,wrapWithCurrentZone:()=>F,filterProperties:()=>[],attachOriginToPatched:()=>F,_redefineProperty:()=>F,patchCallbacks:()=>F};let G={parent:null,zone:new _(null,null)},te=null,re=0;function F(){}r("Zone","Zone"),e.Zone=_}("undefined"!=typeof window&&window||"undefined"!=typeof self&&self||global);const ue=Object.getOwnPropertyDescriptor,he=Object.defineProperty,de=Object.getPrototypeOf,Be=Object.create,ut=Array.prototype.slice,Se="addEventListener",Oe="removeEventListener",Ze=Zone.__symbol__(Se),Ie=Zone.__symbol__(Oe),se="true",ie="false",ke=Zone.__symbol__("");function Le(e,n){return Zone.current.wrap(e,n)}function Me(e,n,i,r,c){return Zone.current.scheduleMacroTask(e,n,i,r,c)}const x=Zone.__symbol__,Pe="undefined"!=typeof window,pe=Pe?window:void 0,$=Pe&&pe||"object"==typeof self&&self||global,ht=[null];function Ae(e,n){for(let i=e.length-1;i>=0;i--)"function"==typeof e[i]&&(e[i]=Le(e[i],n+"_"+i));return e}function Fe(e){return!e||!1!==e.writable&&!("function"==typeof e.get&&void 0===e.set)}const Ue="undefined"!=typeof WorkerGlobalScope&&self instanceof WorkerGlobalScope,Re=!("nw"in $)&&void 0!==$.process&&"[object process]"==={}.toString.call($.process),je=!Re&&!Ue&&!(!Pe||!pe.HTMLElement),We=void 0!==$.process&&"[object process]"==={}.toString.call($.process)&&!Ue&&!(!Pe||!pe.HTMLElement),Ce={},qe=function(e){if(!(e=e||$.event))return;let n=Ce[e.type];n||(n=Ce[e.type]=x("ON_PROPERTY"+e.type));const i=this||e.target||$,r=i[n];let c;if(je&&i===pe&&"error"===e.type){const u=e;c=r&&r.call(this,u.message,u.filename,u.lineno,u.colno,u.error),!0===c&&e.preventDefault()}else c=r&&r.apply(this,arguments),null!=c&&!c&&e.preventDefault();return c};function Xe(e,n,i){let r=ue(e,n);if(!r&&i&&ue(i,n)&&(r={enumerable:!0,configurable:!0}),!r||!r.configurable)return;const c=x("on"+n+"patched");if(e.hasOwnProperty(c)&&e[c])return;delete r.writable,delete r.value;const u=r.get,f=r.set,_=n.substr(2);let y=Ce[_];y||(y=Ce[_]=x("ON_PROPERTY"+_)),r.set=function(T){let m=this;!m&&e===$&&(m=$),m&&(m[y]&&m.removeEventListener(_,qe),f&&f.apply(m,ht),"function"==typeof T?(m[y]=T,m.addEventListener(_,qe,!1)):m[y]=null)},r.get=function(){let T=this;if(!T&&e===$&&(T=$),!T)return null;const m=T[y];if(m)return m;if(u){let S=u&&u.call(this);if(S)return r.set.call(this,S),"function"==typeof T.removeAttribute&&T.removeAttribute(n),S}return null},he(e,n,r),e[c]=!0}function Ye(e,n,i){if(n)for(let r=0;r<n.length;r++)Xe(e,"on"+n[r],i);else{const r=[];for(const c in e)"on"==c.substr(0,2)&&r.push(c);for(let c=0;c<r.length;c++)Xe(e,r[c],i)}}const ne=x("originalInstance");function ve(e){const n=$[e];if(!n)return;$[x(e)]=n,$[e]=function(){const c=Ae(arguments,e);switch(c.length){case 0:this[ne]=new n;break;case 1:this[ne]=new n(c[0]);break;case 2:this[ne]=new n(c[0],c[1]);break;case 3:this[ne]=new n(c[0],c[1],c[2]);break;case 4:this[ne]=new n(c[0],c[1],c[2],c[3]);break;default:throw new Error("Arg list too long.")}},ae($[e],n);const i=new n(function(){});let r;for(r in i)"XMLHttpRequest"===e&&"responseBlob"===r||function(c){"function"==typeof i[c]?$[e].prototype[c]=function(){return this[ne][c].apply(this[ne],arguments)}:he($[e].prototype,c,{set:function(u){"function"==typeof u?(this[ne][c]=Le(u,e+"."+c),ae(this[ne][c],u)):this[ne][c]=u},get:function(){return this[ne][c]}})}(r);for(r in n)"prototype"!==r&&n.hasOwnProperty(r)&&($[e][r]=n[r])}function ce(e,n,i){let r=e;for(;r&&!r.hasOwnProperty(n);)r=de(r);!r&&e[n]&&(r=e);const c=x(n);let u=null;if(r&&(!(u=r[c])||!r.hasOwnProperty(c))&&(u=r[c]=r[n],Fe(r&&ue(r,n)))){const _=i(u,c,n);r[n]=function(){return _(this,arguments)},ae(r[n],u)}return u}function _t(e,n,i){let r=null;function c(u){const f=u.data;return f.args[f.cbIdx]=function(){u.invoke.apply(this,arguments)},r.apply(f.target,f.args),u}r=ce(e,n,u=>function(f,_){const y=i(f,_);return y.cbIdx>=0&&"function"==typeof _[y.cbIdx]?Me(y.name,_[y.cbIdx],y,c):u.apply(f,_)})}function ae(e,n){e[x("OriginalDelegate")]=n}let $e=!1,He=!1;function mt(){if($e)return He;$e=!0;try{const e=pe.navigator.userAgent;(-1!==e.indexOf("MSIE ")||-1!==e.indexOf("Trident/")||-1!==e.indexOf("Edge/"))&&(He=!0)}catch(e){}return He}Zone.__load_patch("ZoneAwarePromise",(e,n,i)=>{const r=Object.getOwnPropertyDescriptor,c=Object.defineProperty,f=i.symbol,_=[],y=!0===e[f("DISABLE_WRAPPING_UNCAUGHT_PROMISE_REJECTION")],T=f("Promise"),m=f("then");i.onUnhandledError=l=>{if(i.showUncaughtError()){const s=l&&l.rejection;s?console.error("Unhandled Promise rejection:",s instanceof Error?s.message:s,"; Zone:",l.zone.name,"; Task:",l.task&&l.task.source,"; Value:",s,s instanceof Error?s.stack:void 0):console.error(l)}},i.microtaskDrainDone=()=>{for(;_.length;){const l=_.shift();try{l.zone.runGuarded(()=>{throw l.throwOriginal?l.rejection:l})}catch(s){Z(s)}}};const D=f("unhandledPromiseRejectionHandler");function Z(l){i.onUnhandledError(l);try{const s=n[D];"function"==typeof s&&s.call(this,l)}catch(s){}}function B(l){return l&&l.then}function V(l){return l}function E(l){return t.reject(l)}const d=f("state"),L=f("value"),z=f("finally"),j=f("parentPromiseValue"),q=f("parentPromiseState"),X=null,A=!0,Y=!1;function M(l,s){return a=>{try{G(l,s,a)}catch(h){G(l,!1,h)}}}const le=f("currentTaskTrace");function G(l,s,a){const h=function(){let l=!1;return function(a){return function(){l||(l=!0,a.apply(null,arguments))}}}();if(l===a)throw new TypeError("Promise resolved with itself");if(l[d]===X){let w=null;try{("object"==typeof a||"function"==typeof a)&&(w=a&&a.then)}catch(C){return h(()=>{G(l,!1,C)})(),l}if(s!==Y&&a instanceof t&&a.hasOwnProperty(d)&&a.hasOwnProperty(L)&&a[d]!==X)re(a),G(l,a[d],a[L]);else if(s!==Y&&"function"==typeof w)try{w.call(a,h(M(l,s)),h(M(l,!1)))}catch(C){h(()=>{G(l,!1,C)})()}else{l[d]=s;const C=l[L];if(l[L]=a,l[z]===z&&s===A&&(l[d]=l[q],l[L]=l[j]),s===Y&&a instanceof Error){const k=n.currentTask&&n.currentTask.data&&n.currentTask.data.__creationTrace__;k&&c(a,le,{configurable:!0,enumerable:!1,writable:!0,value:k})}for(let k=0;k<C.length;)F(l,C[k++],C[k++],C[k++],C[k++]);if(0==C.length&&s==Y){l[d]=0;let k=a;try{throw new Error("Uncaught (in promise): "+function(l){return l&&l.toString===Object.prototype.toString?(l.constructor&&l.constructor.name||"")+": "+JSON.stringify(l):l?l.toString():Object.prototype.toString.call(l)}(a)+(a&&a.stack?"\n"+a.stack:""))}catch(b){k=b}y&&(k.throwOriginal=!0),k.rejection=a,k.promise=l,k.zone=n.current,k.task=n.currentTask,_.push(k),i.scheduleMicroTask()}}}return l}const te=f("rejectionHandledHandler");function re(l){if(0===l[d]){try{const s=n[te];s&&"function"==typeof s&&s.call(this,{rejection:l[L],promise:l})}catch(s){}l[d]=Y;for(let s=0;s<_.length;s++)l===_[s].promise&&_.splice(s,1)}}function F(l,s,a,h,w){re(l);const C=l[d],k=C?"function"==typeof h?h:V:"function"==typeof w?w:E;s.scheduleMicroTask("Promise.then",()=>{try{const b=l[L],N=!!a&&z===a[z];N&&(a[j]=b,a[q]=C);const H=s.run(k,void 0,N&&k!==E&&k!==V?[]:[b]);G(a,!0,H)}catch(b){G(a,!1,b)}},a)}const p=function(){};class t{static toString(){return"function ZoneAwarePromise() { [native code] }"}static resolve(s){return G(new this(null),A,s)}static reject(s){return G(new this(null),Y,s)}static race(s){let a,h,w=new this((b,N)=>{a=b,h=N});function C(b){a(b)}function k(b){h(b)}for(let b of s)B(b)||(b=this.resolve(b)),b.then(C,k);return w}static all(s){return t.allWithCallback(s)}static allSettled(s){return(this&&this.prototype instanceof t?this:t).allWithCallback(s,{thenCallback:h=>({status:"fulfilled",value:h}),errorCallback:h=>({status:"rejected",reason:h})})}static allWithCallback(s,a){let h,w,C=new this((H,U)=>{h=H,w=U}),k=2,b=0;const N=[];for(let H of s){B(H)||(H=this.resolve(H));const U=b;try{H.then(Q=>{N[U]=a?a.thenCallback(Q):Q,k--,0===k&&h(N)},Q=>{a?(N[U]=a.errorCallback(Q),k--,0===k&&h(N)):w(Q)})}catch(Q){w(Q)}k++,b++}return k-=2,0===k&&h(N),C}constructor(s){const a=this;if(!(a instanceof t))throw new Error("Must be an instanceof Promise.");a[d]=X,a[L]=[];try{s&&s(M(a,A),M(a,Y))}catch(h){G(a,!1,h)}}get[Symbol.toStringTag](){return"Promise"}get[Symbol.species](){return t}then(s,a){let h=this.constructor[Symbol.species];(!h||"function"!=typeof h)&&(h=this.constructor||t);const w=new h(p),C=n.current;return this[d]==X?this[L].push(C,w,s,a):F(this,C,w,s,a),w}catch(s){return this.then(null,s)}finally(s){let a=this.constructor[Symbol.species];(!a||"function"!=typeof a)&&(a=t);const h=new a(p);h[z]=z;const w=n.current;return this[d]==X?this[L].push(w,h,s,s):F(this,w,h,s,s),h}}t.resolve=t.resolve,t.reject=t.reject,t.race=t.race,t.all=t.all;const o=e[T]=e.Promise;e.Promise=t;const g=f("thenPatched");function P(l){const s=l.prototype,a=r(s,"then");if(a&&(!1===a.writable||!a.configurable))return;const h=s.then;s[m]=h,l.prototype.then=function(w,C){return new t((b,N)=>{h.call(this,b,N)}).then(w,C)},l[g]=!0}return i.patchThen=P,o&&(P(o),ce(e,"fetch",l=>function(l){return function(s,a){let h=l.apply(s,a);if(h instanceof t)return h;let w=h.constructor;return w[g]||P(w),h}}(l))),Promise[n.__symbol__("uncaughtPromiseErrors")]=_,t}),Zone.__load_patch("toString",e=>{const n=Function.prototype.toString,i=x("OriginalDelegate"),r=x("Promise"),c=x("Error"),u=function(){if("function"==typeof this){const T=this[i];if(T)return"function"==typeof T?n.call(T):Object.prototype.toString.call(T);if(this===Promise){const m=e[r];if(m)return n.call(m)}if(this===Error){const m=e[c];if(m)return n.call(m)}}return n.call(this)};u[i]=n,Function.prototype.toString=u;const f=Object.prototype.toString;Object.prototype.toString=function(){return"function"==typeof Promise&&this instanceof Promise?"[object Promise]":f.call(this)}});let me=!1;if("undefined"!=typeof window)try{const e=Object.defineProperty({},"passive",{get:function(){me=!0}});window.addEventListener("test",e,e),window.removeEventListener("test",e,e)}catch(e){me=!1}const Et={useG:!0},ee={},Ke={},Je=new RegExp("^"+ke+"(\\w+)(true|false)$"),xe=x("propagationStopped");function Qe(e,n){const i=(n?n(e):e)+ie,r=(n?n(e):e)+se,c=ke+i,u=ke+r;ee[e]={},ee[e][ie]=c,ee[e][se]=u}function Tt(e,n,i){const r=i&&i.add||Se,c=i&&i.rm||Oe,u=i&&i.listeners||"eventListeners",f=i&&i.rmAll||"removeAllListeners",_=x(r),y="."+r+":",S=function(E,d,L){if(E.isRemoved)return;const z=E.callback;"object"==typeof z&&z.handleEvent&&(E.callback=q=>z.handleEvent(q),E.originalDelegate=z),E.invoke(E,d,[L]);const j=E.options;j&&"object"==typeof j&&j.once&&d[c].call(d,L.type,E.originalDelegate?E.originalDelegate:E.callback,j)},D=function(E){if(!(E=E||e.event))return;const d=this||E.target||e,L=d[ee[E.type][ie]];if(L)if(1===L.length)S(L[0],d,E);else{const z=L.slice();for(let j=0;j<z.length&&(!E||!0!==E[xe]);j++)S(z[j],d,E)}},Z=function(E){if(!(E=E||e.event))return;const d=this||E.target||e,L=d[ee[E.type][se]];if(L)if(1===L.length)S(L[0],d,E);else{const z=L.slice();for(let j=0;j<z.length&&(!E||!0!==E[xe]);j++)S(z[j],d,E)}};function B(E,d){if(!E)return!1;let L=!0;d&&void 0!==d.useG&&(L=d.useG);const z=d&&d.vh;let j=!0;d&&void 0!==d.chkDup&&(j=d.chkDup);let q=!1;d&&void 0!==d.rt&&(q=d.rt);let O=E;for(;O&&!O.hasOwnProperty(r);)O=de(O);if(!O&&E[r]&&(O=E),!O||O[_])return!1;const X=d&&d.eventNameToString,A={},Y=O[_]=O[r],v=O[x(c)]=O[c],M=O[x(u)]=O[u],R=O[x(f)]=O[f];let J;function le(s,a){return!me&&"object"==typeof s&&s?!!s.capture:me&&a?"boolean"==typeof s?{capture:s,passive:!0}:s?"object"==typeof s&&!1!==s.passive?Object.assign(Object.assign({},s),{passive:!0}):s:{passive:!0}:s}d&&d.prepend&&(J=O[x(d.prepend)]=O[d.prepend]);const p=L?function(s){if(!A.isExisting)return Y.call(A.target,A.eventName,A.capture?Z:D,A.options)}:function(s){return Y.call(A.target,A.eventName,s.invoke,A.options)},t=L?function(s){if(!s.isRemoved){const a=ee[s.eventName];let h;a&&(h=a[s.capture?se:ie]);const w=h&&s.target[h];if(w)for(let C=0;C<w.length;C++)if(w[C]===s){w.splice(C,1),s.isRemoved=!0,0===w.length&&(s.allRemoved=!0,s.target[h]=null);break}}if(s.allRemoved)return v.call(s.target,s.eventName,s.capture?Z:D,s.options)}:function(s){return v.call(s.target,s.eventName,s.invoke,s.options)},g=d&&d.diff?d.diff:function(s,a){const h=typeof a;return"function"===h&&s.callback===a||"object"===h&&s.originalDelegate===a},P=Zone[x("UNPATCHED_EVENTS")],K=e[x("PASSIVE_EVENTS")],l=function(s,a,h,w,C=!1,k=!1){return function(){const b=this||e;let N=arguments[0];d&&d.transferEventName&&(N=d.transferEventName(N));let H=arguments[1];if(!H)return s.apply(this,arguments);if(Re&&"uncaughtException"===N)return s.apply(this,arguments);let U=!1;if("function"!=typeof H){if(!H.handleEvent)return s.apply(this,arguments);U=!0}if(z&&!z(s,H,b,arguments))return;const Q=me&&!!K&&-1!==K.indexOf(N),oe=le(arguments[2],Q);if(P)for(let _e=0;_e<P.length;_e++)if(N===P[_e])return Q?s.call(b,N,H,oe):s.apply(this,arguments);const Ge=!!oe&&("boolean"==typeof oe||oe.capture),st=!(!oe||"object"!=typeof oe)&&oe.once,At=Zone.current;let ze=ee[N];ze||(Qe(N,X),ze=ee[N]);const it=ze[Ge?se:ie];let De,ye=b[it],ct=!1;if(ye){if(ct=!0,j)for(let _e=0;_e<ye.length;_e++)if(g(ye[_e],H))return}else ye=b[it]=[];const at=b.constructor.name,lt=Ke[at];lt&&(De=lt[N]),De||(De=at+a+(X?X(N):N)),A.options=oe,st&&(A.options.once=!1),A.target=b,A.capture=Ge,A.eventName=N,A.isExisting=ct;const be=L?Et:void 0;be&&(be.taskData=A);const fe=At.scheduleEventTask(De,H,be,h,w);return A.target=null,be&&(be.taskData=null),st&&(oe.once=!0),!me&&"boolean"==typeof fe.options||(fe.options=oe),fe.target=b,fe.capture=Ge,fe.eventName=N,U&&(fe.originalDelegate=H),k?ye.unshift(fe):ye.push(fe),C?b:void 0}};return O[r]=l(Y,y,p,t,q),J&&(O.prependListener=l(J,".prependListener:",function(s){return J.call(A.target,A.eventName,s.invoke,A.options)},t,q,!0)),O[c]=function(){const s=this||e;let a=arguments[0];d&&d.transferEventName&&(a=d.transferEventName(a));const h=arguments[2],w=!!h&&("boolean"==typeof h||h.capture),C=arguments[1];if(!C)return v.apply(this,arguments);if(z&&!z(v,C,s,arguments))return;const k=ee[a];let b;k&&(b=k[w?se:ie]);const N=b&&s[b];if(N)for(let H=0;H<N.length;H++){const U=N[H];if(g(U,C))return N.splice(H,1),U.isRemoved=!0,0===N.length&&(U.allRemoved=!0,s[b]=null,"string"==typeof a)&&(s[ke+"ON_PROPERTY"+a]=null),U.zone.cancelTask(U),q?s:void 0}return v.apply(this,arguments)},O[u]=function(){const s=this||e;let a=arguments[0];d&&d.transferEventName&&(a=d.transferEventName(a));const h=[],w=et(s,X?X(a):a);for(let C=0;C<w.length;C++){const k=w[C];h.push(k.originalDelegate?k.originalDelegate:k.callback)}return h},O[f]=function(){const s=this||e;let a=arguments[0];if(a){d&&d.transferEventName&&(a=d.transferEventName(a));const h=ee[a];if(h){const k=s[h[ie]],b=s[h[se]];if(k){const N=k.slice();for(let H=0;H<N.length;H++){const U=N[H];this[c].call(this,a,U.originalDelegate?U.originalDelegate:U.callback,U.options)}}if(b){const N=b.slice();for(let H=0;H<N.length;H++){const U=N[H];this[c].call(this,a,U.originalDelegate?U.originalDelegate:U.callback,U.options)}}}}else{const h=Object.keys(s);for(let w=0;w<h.length;w++){const k=Je.exec(h[w]);let b=k&&k[1];b&&"removeListener"!==b&&this[f].call(this,b)}this[f].call(this,"removeListener")}if(q)return this},ae(O[r],Y),ae(O[c],v),R&&ae(O[f],R),M&&ae(O[u],M),!0}let V=[];for(let E=0;E<n.length;E++)V[E]=B(n[E],i);return V}function et(e,n){if(!n){const u=[];for(let f in e){const _=Je.exec(f);let y=_&&_[1];if(y&&(!n||y===n)){const T=e[f];if(T)for(let m=0;m<T.length;m++)u.push(T[m])}}return u}let i=ee[n];i||(Qe(n),i=ee[n]);const r=e[i[ie]],c=e[i[se]];return r?c?r.concat(c):r.slice():c?c.slice():[]}function gt(e,n){const i=e.Event;i&&i.prototype&&n.patchMethod(i.prototype,"stopImmediatePropagation",r=>function(c,u){c[xe]=!0,r&&r.apply(c,u)})}function yt(e,n,i,r,c){const u=Zone.__symbol__(r);if(n[u])return;const f=n[u]=n[r];n[r]=function(_,y,T){return y&&y.prototype&&c.forEach(function(m){const S=`${i}.${r}::`+m,D=y.prototype;if(D.hasOwnProperty(m)){const Z=e.ObjectGetOwnPropertyDescriptor(D,m);Z&&Z.value?(Z.value=e.wrapWithCurrentZone(Z.value,S),e._redefineProperty(y.prototype,m,Z)):D[m]&&(D[m]=e.wrapWithCurrentZone(D[m],S))}else D[m]&&(D[m]=e.wrapWithCurrentZone(D[m],S))}),f.call(n,_,y,T)},e.attachOriginToPatched(n[r],f)}const Ve=["absolutedeviceorientation","afterinput","afterprint","appinstalled","beforeinstallprompt","beforeprint","beforeunload","devicelight","devicemotion","deviceorientation","deviceorientationabsolute","deviceproximity","hashchange","languagechange","message","mozbeforepaint","offline","online","paint","pageshow","pagehide","popstate","rejectionhandled","storage","unhandledrejection","unload","userproximity","vrdisplayconnected","vrdisplaydisconnected","vrdisplaypresentchange"],wt=["encrypted","waitingforkey","msneedkey","mozinterruptbegin","mozinterruptend"],tt=["load"],nt=["blur","error","focus","load","resize","scroll","messageerror"],Dt=["bounce","finish","start"],rt=["loadstart","progress","abort","error","load","progress","timeout","loadend","readystatechange"],Ee=["upgradeneeded","complete","abort","success","error","blocked","versionchange","close"],St=["close","error","open","message"],Ot=["error","message"],Te=["abort","animationcancel","animationend","animationiteration","auxclick","beforeinput","blur","cancel","canplay","canplaythrough","change","compositionstart","compositionupdate","compositionend","cuechange","click","close","contextmenu","curechange","dblclick","drag","dragend","dragenter","dragexit","dragleave","dragover","drop","durationchange","emptied","ended","error","focus","focusin","focusout","gotpointercapture","input","invalid","keydown","keypress","keyup","load","loadstart","loadeddata","loadedmetadata","lostpointercapture","mousedown","mouseenter","mouseleave","mousemove","mouseout","mouseover","mouseup","mousewheel","orientationchange","pause","play","playing","pointercancel","pointerdown","pointerenter","pointerleave","pointerlockchange","mozpointerlockchange","webkitpointerlockerchange","pointerlockerror","mozpointerlockerror","webkitpointerlockerror","pointermove","pointout","pointerover","pointerup","progress","ratechange","reset","resize","scroll","seeked","seeking","select","selectionchange","selectstart","show","sort","stalled","submit","suspend","timeupdate","volumechange","touchcancel","touchmove","touchstart","touchend","transitioncancel","transitionend","waiting","wheel"].concat(["webglcontextrestored","webglcontextlost","webglcontextcreationerror"],["autocomplete","autocompleteerror"],["toggle"],["afterscriptexecute","beforescriptexecute","DOMContentLoaded","freeze","fullscreenchange","mozfullscreenchange","webkitfullscreenchange","msfullscreenchange","fullscreenerror","mozfullscreenerror","webkitfullscreenerror","msfullscreenerror","readystatechange","visibilitychange","resume"],Ve,["beforecopy","beforecut","beforepaste","copy","cut","paste","dragstart","loadend","animationstart","search","transitionrun","transitionstart","webkitanimationend","webkitanimationiteration","webkitanimationstart","webkittransitionend"],["activate","afterupdate","ariarequest","beforeactivate","beforedeactivate","beforeeditfocus","beforeupdate","cellchange","controlselect","dataavailable","datasetchanged","datasetcomplete","errorupdate","filterchange","layoutcomplete","losecapture","move","moveend","movestart","propertychange","resizeend","resizestart","rowenter","rowexit","rowsdelete","rowsinserted","command","compassneedscalibration","deactivate","help","mscontentzoom","msmanipulationstatechanged","msgesturechange","msgesturedoubletap","msgestureend","msgesturehold","msgesturestart","msgesturetap","msgotpointercapture","msinertiastart","mslostpointercapture","mspointercancel","mspointerdown","mspointerenter","mspointerhover","mspointerleave","mspointermove","mspointerout","mspointerover","mspointerup","pointerout","mssitemodejumplistitemremoved","msthumbnailclick","stop","storagecommit"]);function ot(e,n,i){if(!i||0===i.length)return n;const r=i.filter(u=>u.target===e);if(!r||0===r.length)return n;const c=r[0].ignoreProperties;return n.filter(u=>-1===c.indexOf(u))}function W(e,n,i,r){e&&Ye(e,ot(e,n,i),r)}Zone.__load_patch("util",(e,n,i)=>{i.patchOnProperties=Ye,i.patchMethod=ce,i.bindArguments=Ae,i.patchMacroTask=_t;const r=n.__symbol__("BLACK_LISTED_EVENTS"),c=n.__symbol__("UNPATCHED_EVENTS");e[c]&&(e[r]=e[c]),e[r]&&(n[r]=n[c]=e[r]),i.patchEventPrototype=gt,i.patchEventTarget=Tt,i.isIEOrEdge=mt,i.ObjectDefineProperty=he,i.ObjectGetOwnPropertyDescriptor=ue,i.ObjectCreate=Be,i.ArraySlice=ut,i.patchClass=ve,i.wrapWithCurrentZone=Le,i.filterProperties=ot,i.attachOriginToPatched=ae,i._redefineProperty=Object.defineProperty,i.patchCallbacks=yt,i.getGlobalObjects=()=>({globalSources:Ke,zoneSymbolEventNames:ee,eventNames:Te,isBrowser:je,isMix:We,isNode:Re,TRUE_STR:se,FALSE_STR:ie,ZONE_SYMBOL_PREFIX:ke,ADD_EVENT_LISTENER_STR:Se,REMOVE_EVENT_LISTENER_STR:Oe})});const Ne=x("zoneTask");function ge(e,n,i,r){let c=null,u=null;i+=r;const f={};function _(T){const m=T.data;return m.args[0]=function(){return T.invoke.apply(this,arguments)},m.handleId=c.apply(e,m.args),T}function y(T){return u.call(e,T.data.handleId)}c=ce(e,n+=r,T=>function(m,S){if("function"==typeof S[0]){const D={isPeriodic:"Interval"===r,delay:"Timeout"===r||"Interval"===r?S[1]||0:void 0,args:S},Z=S[0];S[0]=function(){try{return Z.apply(this,arguments)}finally{D.isPeriodic||("number"==typeof D.handleId?delete f[D.handleId]:D.handleId&&(D.handleId[Ne]=null))}};const B=Me(n,S[0],D,_,y);if(!B)return B;const V=B.data.handleId;return"number"==typeof V?f[V]=B:V&&(V[Ne]=B),V&&V.ref&&V.unref&&"function"==typeof V.ref&&"function"==typeof V.unref&&(B.ref=V.ref.bind(V),B.unref=V.unref.bind(V)),"number"==typeof V||V?V:B}return T.apply(e,S)}),u=ce(e,i,T=>function(m,S){const D=S[0];let Z;"number"==typeof D?Z=f[D]:(Z=D&&D[Ne],Z||(Z=D)),Z&&"string"==typeof Z.type?"notScheduled"!==Z.state&&(Z.cancelFn&&Z.data.isPeriodic||0===Z.runCount)&&("number"==typeof D?delete f[D]:D&&(D[Ne]=null),Z.zone.cancelTask(Z)):T.apply(e,S)})}Zone.__load_patch("legacy",e=>{const n=e[Zone.__symbol__("legacyPatch")];n&&n()}),Zone.__load_patch("queueMicrotask",(e,n,i)=>{i.patchMethod(e,"queueMicrotask",r=>function(c,u){n.current.scheduleMicroTask("queueMicrotask",u[0])})}),Zone.__load_patch("timers",e=>{const n="set",i="clear";ge(e,n,i,"Timeout"),ge(e,n,i,"Interval"),ge(e,n,i,"Immediate")}),Zone.__load_patch("requestAnimationFrame",e=>{ge(e,"request","cancel","AnimationFrame"),ge(e,"mozRequest","mozCancel","AnimationFrame"),ge(e,"webkitRequest","webkitCancel","AnimationFrame")}),Zone.__load_patch("blocking",(e,n)=>{const i=["alert","prompt","confirm"];for(let r=0;r<i.length;r++)ce(e,i[r],(u,f,_)=>function(y,T){return n.current.run(u,e,T,_)})}),Zone.__load_patch("EventTarget",(e,n,i)=>{(function(e,n){n.patchEventPrototype(e,n)})(e,i),function(e,n){if(Zone[n.symbol("patchEventTarget")])return;const{eventNames:i,zoneSymbolEventNames:r,TRUE_STR:c,FALSE_STR:u,ZONE_SYMBOL_PREFIX:f}=n.getGlobalObjects();for(let y=0;y<i.length;y++){const T=i[y],D=f+(T+u),Z=f+(T+c);r[T]={},r[T][u]=D,r[T][c]=Z}const _=e.EventTarget;_&&_.prototype&&n.patchEventTarget(e,[_&&_.prototype])}(e,i);const r=e.XMLHttpRequestEventTarget;r&&r.prototype&&i.patchEventTarget(e,[r.prototype])}),Zone.__load_patch("MutationObserver",(e,n,i)=>{ve("MutationObserver"),ve("WebKitMutationObserver")}),Zone.__load_patch("IntersectionObserver",(e,n,i)=>{ve("IntersectionObserver")}),Zone.__load_patch("FileReader",(e,n,i)=>{ve("FileReader")}),Zone.__load_patch("on_property",(e,n,i)=>{!function(e,n){if(Re&&!We||Zone[e.symbol("patchEvents")])return;const i="undefined"!=typeof WebSocket,r=n.__Zone_ignore_on_properties;if(je){const f=window,_=function(){try{const e=pe.navigator.userAgent;if(-1!==e.indexOf("MSIE ")||-1!==e.indexOf("Trident/"))return!0}catch(e){}return!1}()?[{target:f,ignoreProperties:["error"]}]:[];W(f,Te.concat(["messageerror"]),r&&r.concat(_),de(f)),W(Document.prototype,Te,r),void 0!==f.SVGElement&&W(f.SVGElement.prototype,Te,r),W(Element.prototype,Te,r),W(HTMLElement.prototype,Te,r),W(HTMLMediaElement.prototype,wt,r),W(HTMLFrameSetElement.prototype,Ve.concat(nt),r),W(HTMLBodyElement.prototype,Ve.concat(nt),r),W(HTMLFrameElement.prototype,tt,r),W(HTMLIFrameElement.prototype,tt,r);const y=f.HTMLMarqueeElement;y&&W(y.prototype,Dt,r);const T=f.Worker;T&&W(T.prototype,Ot,r)}const c=n.XMLHttpRequest;c&&W(c.prototype,rt,r);const u=n.XMLHttpRequestEventTarget;u&&W(u&&u.prototype,rt,r),"undefined"!=typeof IDBIndex&&(W(IDBIndex.prototype,Ee,r),W(IDBRequest.prototype,Ee,r),W(IDBOpenDBRequest.prototype,Ee,r),W(IDBDatabase.prototype,Ee,r),W(IDBTransaction.prototype,Ee,r),W(IDBCursor.prototype,Ee,r)),i&&W(WebSocket.prototype,St,r)}(i,e)}),Zone.__load_patch("customElements",(e,n,i)=>{!function(e,n){const{isBrowser:i,isMix:r}=n.getGlobalObjects();(i||r)&&e.customElements&&"customElements"in e&&n.patchCallbacks(n,e.customElements,"customElements","define",["connectedCallback","disconnectedCallback","adoptedCallback","attributeChangedCallback"])}(e,i)}),Zone.__load_patch("XHR",(e,n)=>{!function(T){const m=T.XMLHttpRequest;if(!m)return;const S=m.prototype;let Z=S[Ze],B=S[Ie];if(!Z){const v=T.XMLHttpRequestEventTarget;if(v){const M=v.prototype;Z=M[Ze],B=M[Ie]}}const V="readystatechange",E="scheduled";function d(v){const M=v.data,R=M.target;R[u]=!1,R[_]=!1;const J=R[c];Z||(Z=R[Ze],B=R[Ie]),J&&B.call(R,V,J);const le=R[c]=()=>{if(R.readyState===R.DONE)if(!M.aborted&&R[u]&&v.state===E){const te=R[n.__symbol__("loadfalse")];if(0!==R.status&&te&&te.length>0){const re=v.invoke;v.invoke=function(){const F=R[n.__symbol__("loadfalse")];for(let I=0;I<F.length;I++)F[I]===v&&F.splice(I,1);!M.aborted&&v.state===E&&re.call(v)},te.push(v)}else v.invoke()}else!M.aborted&&!1===R[u]&&(R[_]=!0)};return Z.call(R,V,le),R[i]||(R[i]=v),A.apply(R,M.args),R[u]=!0,v}function L(){}function z(v){const M=v.data;return M.aborted=!0,Y.apply(M.target,M.args)}const j=ce(S,"open",()=>function(v,M){return v[r]=0==M[2],v[f]=M[1],j.apply(v,M)}),O=x("fetchTaskAborting"),X=x("fetchTaskScheduling"),A=ce(S,"send",()=>function(v,M){if(!0===n.current[X]||v[r])return A.apply(v,M);{const R={target:v,url:v[f],isPeriodic:!1,args:M,aborted:!1},J=Me("XMLHttpRequest.send",L,R,d,z);v&&!0===v[_]&&!R.aborted&&J.state===E&&J.invoke()}}),Y=ce(S,"abort",()=>function(v,M){const R=function(v){return v[i]}(v);if(R&&"string"==typeof R.type){if(null==R.cancelFn||R.data&&R.data.aborted)return;R.zone.cancelTask(R)}else if(!0===n.current[O])return Y.apply(v,M)})}(e);const i=x("xhrTask"),r=x("xhrSync"),c=x("xhrListener"),u=x("xhrScheduled"),f=x("xhrURL"),_=x("xhrErrorBeforeScheduled")}),Zone.__load_patch("geolocation",e=>{e.navigator&&e.navigator.geolocation&&function(e,n){const i=e.constructor.name;for(let r=0;r<n.length;r++){const c=n[r],u=e[c];if(u){if(!Fe(ue(e,c)))continue;e[c]=(_=>{const y=function(){return _.apply(this,Ae(arguments,i+"."+c))};return ae(y,_),y})(u)}}}(e.navigator.geolocation,["getCurrentPosition","watchPosition"])}),Zone.__load_patch("PromiseRejectionEvent",(e,n)=>{function i(r){return function(c){et(e,r).forEach(f=>{const _=e.PromiseRejectionEvent;if(_){const y=new _(r,{promise:c.promise,reason:c.rejection});f.invoke(y)}})}}e.PromiseRejectionEvent&&(n[x("unhandledPromiseRejectionHandler")]=i("unhandledrejection"),n[x("rejectionHandledHandler")]=i("rejectionhandled"))})},443:(we,ue,he)=>{he(273)}},we=>{we(we.s=443)}]);

(self.webpackChunkcoverage_app=self.webpackChunkcoverage_app||[]).push([[179],{255:wo=>{function Mn(Io){return Promise.resolve().then(()=>{var Tn=new Error("Cannot find module '"+Io+"'");throw Tn.code="MODULE_NOT_FOUND",Tn})}Mn.keys=()=>[],Mn.resolve=Mn,Mn.id=255,wo.exports=Mn},15:(wo,Mn,Io)=>{"use strict";function Tn(e){return"function"==typeof e}let ja=!1;const Rt={Promise:void 0,set useDeprecatedSynchronousErrorHandling(e){if(e){const t=new Error;console.warn("DEPRECATED! RxJS was set to use deprecated synchronous error handling behavior by code at: \n"+t.stack)}else ja&&console.log("RxJS: Back to a better error behavior. Thank you. <3");ja=e},get useDeprecatedSynchronousErrorHandling(){return ja}};function _r(e){setTimeout(()=>{throw e},0)}const $i={closed:!0,next(e){},error(e){if(Rt.useDeprecatedSynchronousErrorHandling)throw e;_r(e)},complete(){}},$a=Array.isArray||(e=>e&&"number"==typeof e.length);function Ua(e){return null!==e&&"object"==typeof e}const Ui=(()=>{function e(t){return Error.call(this),this.message=t?`${t.length} errors occurred during unsubscription:\n${t.map((n,r)=>`${r+1}) ${n.toString()}`).join("\n  ")}`:"",this.name="UnsubscriptionError",this.errors=t,this}return e.prototype=Object.create(Error.prototype),e})();class Ee{constructor(t){this.closed=!1,this._parentOrParents=null,this._subscriptions=null,t&&(this._ctorUnsubscribe=!0,this._unsubscribe=t)}unsubscribe(){let t;if(this.closed)return;let{_parentOrParents:n,_ctorUnsubscribe:r,_unsubscribe:o,_subscriptions:i}=this;if(this.closed=!0,this._parentOrParents=null,this._subscriptions=null,n instanceof Ee)n.remove(this);else if(null!==n)for(let s=0;s<n.length;++s)n[s].remove(this);if(Tn(o)){r&&(this._unsubscribe=void 0);try{o.call(this)}catch(s){t=s instanceof Ui?Ud(s.errors):[s]}}if($a(i)){let s=-1,a=i.length;for(;++s<a;){const l=i[s];if(Ua(l))try{l.unsubscribe()}catch(c){t=t||[],c instanceof Ui?t=t.concat(Ud(c.errors)):t.push(c)}}}if(t)throw new Ui(t)}add(t){let n=t;if(!t)return Ee.EMPTY;switch(typeof t){case"function":n=new Ee(t);case"object":if(n===this||n.closed||"function"!=typeof n.unsubscribe)return n;if(this.closed)return n.unsubscribe(),n;if(!(n instanceof Ee)){const i=n;n=new Ee,n._subscriptions=[i]}break;default:throw new Error("unrecognized teardown "+t+" added to Subscription.")}let{_parentOrParents:r}=n;if(null===r)n._parentOrParents=this;else if(r instanceof Ee){if(r===this)return n;n._parentOrParents=[r,this]}else{if(-1!==r.indexOf(this))return n;r.push(this)}const o=this._subscriptions;return null===o?this._subscriptions=[n]:o.push(n),n}remove(t){const n=this._subscriptions;if(n){const r=n.indexOf(t);-1!==r&&n.splice(r,1)}}}var e;function Ud(e){return e.reduce((t,n)=>t.concat(n instanceof Ui?n.errors:n),[])}Ee.EMPTY=((e=new Ee).closed=!0,e);const Gi="function"==typeof Symbol?Symbol("rxSubscriber"):"@@rxSubscriber_"+Math.random();class lt extends Ee{constructor(t,n,r){switch(super(),this.syncErrorValue=null,this.syncErrorThrown=!1,this.syncErrorThrowable=!1,this.isStopped=!1,arguments.length){case 0:this.destination=$i;break;case 1:if(!t){this.destination=$i;break}if("object"==typeof t){t instanceof lt?(this.syncErrorThrowable=t.syncErrorThrowable,this.destination=t,t.add(this)):(this.syncErrorThrowable=!0,this.destination=new Gd(this,t));break}default:this.syncErrorThrowable=!0,this.destination=new Gd(this,t,n,r)}}[Gi](){return this}static create(t,n,r){const o=new lt(t,n,r);return o.syncErrorThrowable=!1,o}next(t){this.isStopped||this._next(t)}error(t){this.isStopped||(this.isStopped=!0,this._error(t))}complete(){this.isStopped||(this.isStopped=!0,this._complete())}unsubscribe(){this.closed||(this.isStopped=!0,super.unsubscribe())}_next(t){this.destination.next(t)}_error(t){this.destination.error(t),this.unsubscribe()}_complete(){this.destination.complete(),this.unsubscribe()}_unsubscribeAndRecycle(){const{_parentOrParents:t}=this;return this._parentOrParents=null,this.unsubscribe(),this.closed=!1,this.isStopped=!1,this._parentOrParents=t,this}}class Gd extends lt{constructor(t,n,r,o){super(),this._parentSubscriber=t;let i,s=this;Tn(n)?i=n:n&&(i=n.next,r=n.error,o=n.complete,n!==$i&&(s=Object.create(n),Tn(s.unsubscribe)&&this.add(s.unsubscribe.bind(s)),s.unsubscribe=this.unsubscribe.bind(this))),this._context=s,this._next=i,this._error=r,this._complete=o}next(t){if(!this.isStopped&&this._next){const{_parentSubscriber:n}=this;Rt.useDeprecatedSynchronousErrorHandling&&n.syncErrorThrowable?this.__tryOrSetError(n,this._next,t)&&this.unsubscribe():this.__tryOrUnsub(this._next,t)}}error(t){if(!this.isStopped){const{_parentSubscriber:n}=this,{useDeprecatedSynchronousErrorHandling:r}=Rt;if(this._error)r&&n.syncErrorThrowable?(this.__tryOrSetError(n,this._error,t),this.unsubscribe()):(this.__tryOrUnsub(this._error,t),this.unsubscribe());else if(n.syncErrorThrowable)r?(n.syncErrorValue=t,n.syncErrorThrown=!0):_r(t),this.unsubscribe();else{if(this.unsubscribe(),r)throw t;_r(t)}}}complete(){if(!this.isStopped){const{_parentSubscriber:t}=this;if(this._complete){const n=()=>this._complete.call(this._context);Rt.useDeprecatedSynchronousErrorHandling&&t.syncErrorThrowable?(this.__tryOrSetError(t,n),this.unsubscribe()):(this.__tryOrUnsub(n),this.unsubscribe())}else this.unsubscribe()}}__tryOrUnsub(t,n){try{t.call(this._context,n)}catch(r){if(this.unsubscribe(),Rt.useDeprecatedSynchronousErrorHandling)throw r;_r(r)}}__tryOrSetError(t,n,r){if(!Rt.useDeprecatedSynchronousErrorHandling)throw new Error("bad call");try{n.call(this._context,r)}catch(o){return Rt.useDeprecatedSynchronousErrorHandling?(t.syncErrorValue=o,t.syncErrorThrown=!0,!0):(_r(o),!0)}return!1}_unsubscribe(){const{_parentSubscriber:t}=this;this._context=null,this._parentSubscriber=null,t.unsubscribe()}}const Mo="function"==typeof Symbol&&Symbol.observable||"@@observable";function zd(e){return e}let qe=(()=>{class e{constructor(n){this._isScalar=!1,n&&(this._subscribe=n)}lift(n){const r=new e;return r.source=this,r.operator=n,r}subscribe(n,r,o){const{operator:i}=this,s=function(e,t,n){if(e){if(e instanceof lt)return e;if(e[Gi])return e[Gi]()}return e||t||n?new lt(e,t,n):new lt($i)}(n,r,o);if(s.add(i?i.call(s,this.source):this.source||Rt.useDeprecatedSynchronousErrorHandling&&!s.syncErrorThrowable?this._subscribe(s):this._trySubscribe(s)),Rt.useDeprecatedSynchronousErrorHandling&&s.syncErrorThrowable&&(s.syncErrorThrowable=!1,s.syncErrorThrown))throw s.syncErrorValue;return s}_trySubscribe(n){try{return this._subscribe(n)}catch(r){Rt.useDeprecatedSynchronousErrorHandling&&(n.syncErrorThrown=!0,n.syncErrorValue=r),function(e){for(;e;){const{closed:t,destination:n,isStopped:r}=e;if(t||r)return!1;e=n&&n instanceof lt?n:null}return!0}(n)?n.error(r):console.warn(r)}}forEach(n,r){return new(r=qd(r))((o,i)=>{let s;s=this.subscribe(a=>{try{n(a)}catch(l){i(l),s&&s.unsubscribe()}},i,o)})}_subscribe(n){const{source:r}=this;return r&&r.subscribe(n)}[Mo](){return this}pipe(...n){return 0===n.length?this:function(e){return 0===e.length?zd:1===e.length?e[0]:function(n){return e.reduce((r,o)=>o(r),n)}}(n)(this)}toPromise(n){return new(n=qd(n))((r,o)=>{let i;this.subscribe(s=>i=s,s=>o(s),()=>r(i))})}}return e.create=t=>new e(t),e})();function qd(e){if(e||(e=Rt.Promise||Promise),!e)throw new Error("no Promise impl found");return e}const To=(()=>{function e(){return Error.call(this),this.message="object unsubscribed",this.name="ObjectUnsubscribedError",this}return e.prototype=Object.create(Error.prototype),e})();class pv extends Ee{constructor(t,n){super(),this.subject=t,this.subscriber=n,this.closed=!1}unsubscribe(){if(this.closed)return;this.closed=!0;const t=this.subject,n=t.observers;if(this.subject=null,!n||0===n.length||t.isStopped||t.closed)return;const r=n.indexOf(this.subscriber);-1!==r&&n.splice(r,1)}}class Qd extends lt{constructor(t){super(t),this.destination=t}}let Ga=(()=>{class e extends qe{constructor(){super(),this.observers=[],this.closed=!1,this.isStopped=!1,this.hasError=!1,this.thrownError=null}[Gi](){return new Qd(this)}lift(n){const r=new Kd(this,this);return r.operator=n,r}next(n){if(this.closed)throw new To;if(!this.isStopped){const{observers:r}=this,o=r.length,i=r.slice();for(let s=0;s<o;s++)i[s].next(n)}}error(n){if(this.closed)throw new To;this.hasError=!0,this.thrownError=n,this.isStopped=!0;const{observers:r}=this,o=r.length,i=r.slice();for(let s=0;s<o;s++)i[s].error(n);this.observers.length=0}complete(){if(this.closed)throw new To;this.isStopped=!0;const{observers:n}=this,r=n.length,o=n.slice();for(let i=0;i<r;i++)o[i].complete();this.observers.length=0}unsubscribe(){this.isStopped=!0,this.closed=!0,this.observers=null}_trySubscribe(n){if(this.closed)throw new To;return super._trySubscribe(n)}_subscribe(n){if(this.closed)throw new To;return this.hasError?(n.error(this.thrownError),Ee.EMPTY):this.isStopped?(n.complete(),Ee.EMPTY):(this.observers.push(n),new pv(this,n))}asObservable(){const n=new qe;return n.source=this,n}}return e.create=(t,n)=>new Kd(t,n),e})();class Kd extends Ga{constructor(t,n){super(),this.destination=t,this.source=n}next(t){const{destination:n}=this;n&&n.next&&n.next(t)}error(t){const{destination:n}=this;n&&n.error&&this.destination.error(t)}complete(){const{destination:t}=this;t&&t.complete&&this.destination.complete()}_subscribe(t){const{source:n}=this;return n?this.source.subscribe(t):Ee.EMPTY}}function za(e,t){return function(r){if("function"!=typeof e)throw new TypeError("argument is not a function. Are you looking for `mapTo()`?");return r.lift(new mv(e,t))}}class mv{constructor(t,n){this.project=t,this.thisArg=n}call(t,n){return n.subscribe(new _v(t,this.project,this.thisArg))}}class _v extends lt{constructor(t,n,r){super(t),this.project=n,this.count=0,this.thisArg=r||this}_next(t){let n;try{n=this.project.call(this.thisArg,t,this.count++)}catch(r){return void this.destination.error(r)}this.destination.next(n)}}const Yd=e=>t=>{for(let n=0,r=e.length;n<r&&!t.closed;n++)t.next(e[n]);t.complete()},zi="function"==typeof Symbol&&Symbol.iterator?Symbol.iterator:"@@iterator",Zd=e=>e&&"number"==typeof e.length&&"function"!=typeof e;function Jd(e){return!!e&&"function"!=typeof e.subscribe&&"function"==typeof e.then}const Xd=e=>{if(e&&"function"==typeof e[Mo])return(e=>t=>{const n=e[Mo]();if("function"!=typeof n.subscribe)throw new TypeError("Provided object does not correctly implement Symbol.observable");return n.subscribe(t)})(e);if(Zd(e))return Yd(e);if(Jd(e))return(e=>t=>(e.then(n=>{t.closed||(t.next(n),t.complete())},n=>t.error(n)).then(null,_r),t))(e);if(e&&"function"==typeof e[zi])return(e=>t=>{const n=e[zi]();for(;;){let r;try{r=n.next()}catch(o){return t.error(o),t}if(r.done){t.complete();break}if(t.next(r.value),t.closed)break}return"function"==typeof n.return&&t.add(()=>{n.return&&n.return()}),t})(e);{const n=`You provided ${Ua(e)?"an invalid object":`'${e}'`} where a stream was expected. You can provide an Observable, Promise, Array, or Iterable.`;throw new TypeError(n)}};function ef(e,t){return new qe(n=>{const r=new Ee;let o=0;return r.add(t.schedule(function(){o!==e.length?(n.next(e[o++]),n.closed||r.add(this.schedule())):n.complete()})),r})}function Wa(e,t){return t?function(e,t){if(null!=e){if(function(e){return e&&"function"==typeof e[Mo]}(e))return function(e,t){return new qe(n=>{const r=new Ee;return r.add(t.schedule(()=>{const o=e[Mo]();r.add(o.subscribe({next(i){r.add(t.schedule(()=>n.next(i)))},error(i){r.add(t.schedule(()=>n.error(i)))},complete(){r.add(t.schedule(()=>n.complete()))}}))})),r})}(e,t);if(Jd(e))return function(e,t){return new qe(n=>{const r=new Ee;return r.add(t.schedule(()=>e.then(o=>{r.add(t.schedule(()=>{n.next(o),r.add(t.schedule(()=>n.complete()))}))},o=>{r.add(t.schedule(()=>n.error(o)))}))),r})}(e,t);if(Zd(e))return ef(e,t);if(function(e){return e&&"function"==typeof e[zi]}(e)||"string"==typeof e)return function(e,t){if(!e)throw new Error("Iterable cannot be null");return new qe(n=>{const r=new Ee;let o;return r.add(()=>{o&&"function"==typeof o.return&&o.return()}),r.add(t.schedule(()=>{o=e[zi](),r.add(t.schedule(function(){if(n.closed)return;let i,s;try{const a=o.next();i=a.value,s=a.done}catch(a){return void n.error(a)}s?n.complete():(n.next(i),this.schedule())}))})),r})}(e,t)}throw new TypeError((null!==e&&typeof e||e)+" is not observable")}(e,t):e instanceof qe?e:new qe(Xd(e))}class Av extends lt{constructor(t){super(),this.parent=t}_next(t){this.parent.notifyNext(t)}_error(t){this.parent.notifyError(t),this.unsubscribe()}_complete(){this.parent.notifyComplete(),this.unsubscribe()}}class Sv extends lt{notifyNext(t){this.destination.next(t)}notifyError(t){this.destination.error(t)}notifyComplete(){this.destination.complete()}}function tf(e,t,n=Number.POSITIVE_INFINITY){return"function"==typeof t?r=>r.pipe(tf((o,i)=>Wa(e(o,i)).pipe(za((s,a)=>t(o,s,i,a))),n)):("number"==typeof t&&(n=t),r=>r.lift(new Nv(e,n)))}class Nv{constructor(t,n=Number.POSITIVE_INFINITY){this.project=t,this.concurrent=n}call(t,n){return n.subscribe(new Rv(t,this.project,this.concurrent))}}class Rv extends Sv{constructor(t,n,r=Number.POSITIVE_INFINITY){super(t),this.project=n,this.concurrent=r,this.hasCompleted=!1,this.buffer=[],this.active=0,this.index=0}_next(t){this.active<this.concurrent?this._tryNext(t):this.buffer.push(t)}_tryNext(t){let n;const r=this.index++;try{n=this.project(t,r)}catch(o){return void this.destination.error(o)}this.active++,this._innerSub(n)}_innerSub(t){const n=new Av(this),r=this.destination;r.add(n);const o=function(e,t){if(t.closed)return;if(e instanceof qe)return e.subscribe(t);let n;try{n=Xd(e)(t)}catch(r){t.error(r)}return n}(t,n);o!==n&&r.add(o)}_complete(){this.hasCompleted=!0,0===this.active&&0===this.buffer.length&&this.destination.complete(),this.unsubscribe()}notifyNext(t){this.destination.next(t)}notifyComplete(){const t=this.buffer;this.active--,t.length>0?this._next(t.shift()):0===this.active&&this.hasCompleted&&this.destination.complete()}}function Fv(e=Number.POSITIVE_INFINITY){return tf(zd,e)}function nf(){return function(t){return t.lift(new Vv(t))}}class Vv{constructor(t){this.connectable=t}call(t,n){const{connectable:r}=this;r._refCount++;const o=new kv(t,r),i=n.subscribe(o);return o.closed||(o.connection=r.connect()),i}}class kv extends lt{constructor(t,n){super(t),this.connectable=n}_unsubscribe(){const{connectable:t}=this;if(!t)return void(this.connection=null);this.connectable=null;const n=t._refCount;if(n<=0)return void(this.connection=null);if(t._refCount=n-1,n>1)return void(this.connection=null);const{connection:r}=this,o=t._connection;this.connection=null,o&&(!r||o===r)&&o.unsubscribe()}}class Lv extends qe{constructor(t,n){super(),this.source=t,this.subjectFactory=n,this._refCount=0,this._isComplete=!1}_subscribe(t){return this.getSubject().subscribe(t)}getSubject(){const t=this._subject;return(!t||t.isStopped)&&(this._subject=this.subjectFactory()),this._subject}connect(){let t=this._connection;return t||(this._isComplete=!1,t=this._connection=new Ee,t.add(this.source.subscribe(new Hv(this.getSubject(),this))),t.closed&&(this._connection=null,t=Ee.EMPTY)),t}refCount(){return nf()(this)}}const Bv=(()=>{const e=Lv.prototype;return{operator:{value:null},_refCount:{value:0,writable:!0},_subject:{value:null,writable:!0},_connection:{value:null,writable:!0},_subscribe:{value:e._subscribe},_isComplete:{value:e._isComplete,writable:!0},getSubject:{value:e.getSubject},connect:{value:e.connect},refCount:{value:e.refCount}}})();class Hv extends Qd{constructor(t,n){super(t),this.connectable=n}_error(t){this._unsubscribe(),super._error(t)}_complete(){this.connectable._isComplete=!0,this._unsubscribe(),super._complete()}_unsubscribe(){const t=this.connectable;if(t){this.connectable=null;const n=t._connection;t._refCount=0,t._subject=null,t._connection=null,n&&n.unsubscribe()}}}function Gv(){return new Ga}function ee(e){for(let t in e)if(e[t]===ee)return t;throw Error("Could not find renamed property on target object.")}function qa(e,t){for(const n in t)t.hasOwnProperty(n)&&!e.hasOwnProperty(n)&&(e[n]=t[n])}function W(e){if("string"==typeof e)return e;if(Array.isArray(e))return"["+e.map(W).join(", ")+"]";if(null==e)return""+e;if(e.overriddenName)return`${e.overriddenName}`;if(e.name)return`${e.name}`;const t=e.toString();if(null==t)return""+t;const n=t.indexOf("\n");return-1===n?t:t.substring(0,n)}function Qa(e,t){return null==e||""===e?null===t?"":t:null==t||""===t?e:e+" "+t}const Wv=ee({__forward_ref__:ee});function ue(e){return e.__forward_ref__=ue,e.toString=function(){return W(this())},e}function N(e){return rf(e)?e():e}function rf(e){return"function"==typeof e&&e.hasOwnProperty(Wv)&&e.__forward_ref__===ue}class Qn extends Error{constructor(t,n){super(function(e,t){return`${e?`NG0${e}: `:""}${t}`}(t,n)),this.code=t}}function U(e){return"string"==typeof e?e:null==e?"":String(e)}function Qe(e){return"function"==typeof e?e.name||e.toString():"object"==typeof e&&null!=e&&"function"==typeof e.type?e.type.name||e.type.toString():U(e)}function Wi(e,t){const n=t?` in ${t}`:"";throw new Qn("201",`No provider for ${Qe(e)} found${n}`)}function ut(e,t){null==e&&function(e,t,n,r){throw new Error(`ASSERTION ERROR: ${e}`+(null==r?"":` [Expected=> ${n} ${r} ${t} <=Actual]`))}(t,e,null,"!=")}function te(e){return{token:e.token,providedIn:e.providedIn||null,factory:e.factory,value:void 0}}function Ft(e){return{providers:e.providers||[],imports:e.imports||[]}}function pn(e){return of(e,qi)||of(e,af)}function of(e,t){return e.hasOwnProperty(t)?e[t]:null}function sf(e){return e&&(e.hasOwnProperty(Ya)||e.hasOwnProperty(Xv))?e[Ya]:null}const qi=ee({\u0275prov:ee}),Ya=ee({\u0275inj:ee}),af=ee({ngInjectableDef:ee}),Xv=ee({ngInjectorDef:ee});var O=(()=>((O=O||{})[O.Default=0]="Default",O[O.Host=1]="Host",O[O.Self=2]="Self",O[O.SkipSelf=4]="SkipSelf",O[O.Optional=8]="Optional",O))();let Za;function An(e){const t=Za;return Za=e,t}function lf(e,t,n){const r=pn(e);return r&&"root"==r.providedIn?void 0===r.value?r.value=r.factory():r.value:n&O.Optional?null:void 0!==t?t:void Wi(W(e),"Injector")}function Sn(e){return{toString:e}.toString()}var yt=(()=>((yt=yt||{})[yt.OnPush=0]="OnPush",yt[yt.Default=1]="Default",yt))(),Se=(()=>((Se=Se||{})[Se.Emulated=0]="Emulated",Se[Se.None=2]="None",Se[Se.ShadowDom=3]="ShadowDom",Se))();const tD="undefined"!=typeof globalThis&&globalThis,nD="undefined"!=typeof window&&window,rD="undefined"!=typeof self&&"undefined"!=typeof WorkerGlobalScope&&self instanceof WorkerGlobalScope&&self,oD="undefined"!=typeof global&&global,ne=tD||oD||nD||rD,yr={},ie=[],Qi=ee({\u0275cmp:ee}),Ja=ee({\u0275dir:ee}),Xa=ee({\u0275pipe:ee}),cf=ee({\u0275mod:ee}),iD=ee({\u0275loc:ee}),gn=ee({\u0275fac:ee}),Ao=ee({__NG_ELEMENT_ID__:ee});let sD=0;function xn(e){return Sn(()=>{const n={},r={type:e.type,providersResolver:null,decls:e.decls,vars:e.vars,factory:null,template:e.template||null,consts:e.consts||null,ngContentSelectors:e.ngContentSelectors,hostBindings:e.hostBindings||null,hostVars:e.hostVars||0,hostAttrs:e.hostAttrs||null,contentQueries:e.contentQueries||null,declaredInputs:n,inputs:null,outputs:null,exportAs:e.exportAs||null,onPush:e.changeDetection===yt.OnPush,directiveDefs:null,pipeDefs:null,selectors:e.selectors||ie,viewQuery:e.viewQuery||null,features:e.features||null,data:e.data||{},encapsulation:e.encapsulation||Se.Emulated,id:"c",styles:e.styles||ie,_:null,setInput:null,schemas:e.schemas||null,tView:null},o=e.directives,i=e.features,s=e.pipes;return r.id+=sD++,r.inputs=hf(e.inputs,n),r.outputs=hf(e.outputs),i&&i.forEach(a=>a(r)),r.directiveDefs=o?()=>("function"==typeof o?o():o).map(uf):null,r.pipeDefs=s?()=>("function"==typeof s?s():s).map(df):null,r})}function uf(e){return Ke(e)||function(e){return e[Ja]||null}(e)}function df(e){return function(e){return e[Xa]||null}(e)}const ff={};function mn(e){return Sn(()=>{const t={type:e.type,bootstrap:e.bootstrap||ie,declarations:e.declarations||ie,imports:e.imports||ie,exports:e.exports||ie,transitiveCompileScopes:null,schemas:e.schemas||null,id:e.id||null};return null!=e.id&&(ff[e.id]=e.type),t})}function hf(e,t){if(null==e)return yr;const n={};for(const r in e)if(e.hasOwnProperty(r)){let o=e[r],i=o;Array.isArray(o)&&(i=o[1],o=o[0]),n[o]=r,t&&(t[o]=i)}return n}const L=xn;function ot(e){return{type:e.type,name:e.name,factory:null,pure:!1!==e.pure,onDestroy:e.type.prototype.ngOnDestroy||null}}function Ke(e){return e[Qi]||null}function Ct(e,t){const n=e[cf]||null;if(!n&&!0===t)throw new Error(`Type ${W(e)} does not have '\u0275mod' property.`);return n}const G=11;function Yt(e){return Array.isArray(e)&&"object"==typeof e[1]}function Pt(e){return Array.isArray(e)&&!0===e[1]}function nl(e){return 0!=(8&e.flags)}function Ji(e){return 2==(2&e.flags)}function Xi(e){return 1==(1&e.flags)}function Vt(e){return null!==e.template}function hD(e){return 0!=(512&e[2])}function Xn(e,t){return e.hasOwnProperty(gn)?e[gn]:null}class gf{constructor(t,n,r){this.previousValue=t,this.currentValue=n,this.firstChange=r}isFirstChange(){return this.firstChange}}function ft(){return mf}function mf(e){return e.type.prototype.ngOnChanges&&(e.setInput=_D),mD}function mD(){const e=yf(this),t=null==e?void 0:e.current;if(t){const n=e.previous;if(n===yr)e.previous=t;else for(let r in t)n[r]=t[r];e.current=null,this.ngOnChanges(t)}}function _D(e,t,n,r){const o=yf(e)||function(e,t){return e[_f]=t}(e,{previous:yr,current:null}),i=o.current||(o.current={}),s=o.previous,a=this.declaredInputs[n],l=s[a];i[a]=new gf(l&&l.currentValue,t,s===yr),e[r]=t}ft.ngInherit=!0;const _f="__ngSimpleChanges__";function yf(e){return e[_f]||null}const Cf="http://www.w3.org/2000/svg";let il;function ve(e){return!!e.listen}const Df={createRenderer:(e,t)=>void 0!==il?il:"undefined"!=typeof document?document:void 0};function Me(e){for(;Array.isArray(e);)e=e[0];return e}function es(e,t){return Me(t[e])}function bt(e,t){return Me(t[e.index])}function al(e,t){return e.data[t]}function ht(e,t){const n=t[e];return Yt(n)?n:n[0]}function ll(e){return 128==(128&e[2])}function Rn(e,t){return null==t?null:e[t]}function Ef(e){e[18]=0}function cl(e,t){e[5]+=t;let n=e,r=e[3];for(;null!==r&&(1===t&&1===n[5]||-1===t&&0===n[5]);)r[5]+=t,n=r,r=r[3]}const B={lFrame:Nf(null),bindingsEnabled:!0,isInCheckNoChangesMode:!1};function wf(){return B.bindingsEnabled}function b(){return B.lFrame.lView}function J(){return B.lFrame.tView}function le(e){return B.lFrame.contextLView=e,e[8]}function xe(){let e=If();for(;null!==e&&64===e.type;)e=e.parent;return e}function If(){return B.lFrame.currentTNode}function Zt(e,t){const n=B.lFrame;n.currentTNode=e,n.isParent=t}function ul(){return B.lFrame.isParent}function dl(){B.lFrame.isParent=!1}function ts(){return B.isInCheckNoChangesMode}function ns(e){B.isInCheckNoChangesMode=e}function Ye(){const e=B.lFrame;let t=e.bindingRootIndex;return-1===t&&(t=e.bindingRootIndex=e.tView.bindingStartIndex),t}function wr(){return B.lFrame.bindingIndex++}function _n(e){const t=B.lFrame,n=t.bindingIndex;return t.bindingIndex=t.bindingIndex+e,n}function RD(e,t){const n=B.lFrame;n.bindingIndex=n.bindingRootIndex=e,fl(t)}function fl(e){B.lFrame.currentDirectiveIndex=e}function pl(e){B.lFrame.currentQueryIndex=e}function OD(e){const t=e[1];return 2===t.type?t.declTNode:1===t.type?e[6]:null}function Sf(e,t,n){if(n&O.SkipSelf){let o=t,i=e;for(;!(o=o.parent,null!==o||n&O.Host||(o=OD(i),null===o||(i=i[15],10&o.type))););if(null===o)return!1;t=o,e=i}const r=B.lFrame=xf();return r.currentTNode=t,r.lView=e,!0}function rs(e){const t=xf(),n=e[1];B.lFrame=t,t.currentTNode=n.firstChild,t.lView=e,t.tView=n,t.contextLView=e,t.bindingIndex=n.bindingStartIndex,t.inI18n=!1}function xf(){const e=B.lFrame,t=null===e?null:e.child;return null===t?Nf(e):t}function Nf(e){const t={currentTNode:null,isParent:!0,lView:null,tView:null,selectedIndex:-1,contextLView:null,elementDepthCount:0,currentNamespace:null,currentDirectiveIndex:-1,bindingRootIndex:-1,bindingIndex:-1,currentQueryIndex:0,parent:e,child:null,inI18n:!1};return null!==e&&(e.child=t),t}function Rf(){const e=B.lFrame;return B.lFrame=e.parent,e.currentTNode=null,e.lView=null,e}const Ff=Rf;function os(){const e=Rf();e.isParent=!0,e.tView=null,e.selectedIndex=-1,e.contextLView=null,e.elementDepthCount=0,e.currentDirectiveIndex=-1,e.currentNamespace=null,e.bindingRootIndex=-1,e.bindingIndex=-1,e.currentQueryIndex=0}function Ze(){return B.lFrame.selectedIndex}function Fn(e){B.lFrame.selectedIndex=e}function De(){const e=B.lFrame;return al(e.tView,e.selectedIndex)}function is(e,t){for(let n=t.directiveStart,r=t.directiveEnd;n<r;n++){const i=e.data[n].type.prototype,{ngAfterContentInit:s,ngAfterContentChecked:a,ngAfterViewInit:l,ngAfterViewChecked:c,ngOnDestroy:u}=i;s&&(e.contentHooks||(e.contentHooks=[])).push(-n,s),a&&((e.contentHooks||(e.contentHooks=[])).push(n,a),(e.contentCheckHooks||(e.contentCheckHooks=[])).push(n,a)),l&&(e.viewHooks||(e.viewHooks=[])).push(-n,l),c&&((e.viewHooks||(e.viewHooks=[])).push(n,c),(e.viewCheckHooks||(e.viewCheckHooks=[])).push(n,c)),null!=u&&(e.destroyHooks||(e.destroyHooks=[])).push(n,u)}}function ss(e,t,n){Pf(e,t,3,n)}function as(e,t,n,r){(3&e[2])===n&&Pf(e,t,n,r)}function gl(e,t){let n=e[2];(3&n)===t&&(n&=2047,n+=1,e[2]=n)}function Pf(e,t,n,r){const i=null!=r?r:-1,s=t.length-1;let a=0;for(let l=void 0!==r?65535&e[18]:0;l<s;l++)if("number"==typeof t[l+1]){if(a=t[l],null!=r&&a>=r)break}else t[l]<0&&(e[18]+=65536),(a<i||-1==i)&&($D(e,n,t,l),e[18]=(4294901760&e[18])+l+2),l++}function $D(e,t,n,r){const o=n[r]<0,i=n[r+1],a=e[o?-n[r]:n[r]];if(o){if(e[2]>>11<e[18]>>16&&(3&e[2])===t){e[2]+=2048;try{i.call(a)}finally{}}}else try{i.call(a)}finally{}}class Fo{constructor(t,n,r){this.factory=t,this.resolving=!1,this.canSeeViewProviders=n,this.injectImpl=r}}function ls(e,t,n){const r=ve(e);let o=0;for(;o<n.length;){const i=n[o];if("number"==typeof i){if(0!==i)break;o++;const s=n[o++],a=n[o++],l=n[o++];r?e.setAttribute(t,a,l,s):t.setAttributeNS(s,a,l)}else{const s=i,a=n[++o];_l(s)?r&&e.setProperty(t,s,a):r?e.setAttribute(t,s,a):t.setAttribute(s,a),o++}}return o}function Vf(e){return 3===e||4===e||6===e}function _l(e){return 64===e.charCodeAt(0)}function cs(e,t){if(null!==t&&0!==t.length)if(null===e||0===e.length)e=t.slice();else{let n=-1;for(let r=0;r<t.length;r++){const o=t[r];"number"==typeof o?n=o:0===n||kf(e,n,o,null,-1===n||2===n?t[++r]:null)}}return e}function kf(e,t,n,r,o){let i=0,s=e.length;if(-1===t)s=-1;else for(;i<e.length;){const a=e[i++];if("number"==typeof a){if(a===t){s=-1;break}if(a>t){s=i-1;break}}}for(;i<e.length;){const a=e[i];if("number"==typeof a)break;if(a===n){if(null===r)return void(null!==o&&(e[i+1]=o));if(r===e[i+1])return void(e[i+2]=o)}i++,null!==r&&i++,null!==o&&i++}-1!==s&&(e.splice(s,0,t),i=s+1),e.splice(i++,0,n),null!==r&&e.splice(i++,0,r),null!==o&&e.splice(i++,0,o)}function Lf(e){return-1!==e}function Ir(e){return 32767&e}function Mr(e,t){let n=function(e){return e>>16}(e),r=t;for(;n>0;)r=r[15],n--;return r}let yl=!0;function us(e){const t=yl;return yl=e,t}let QD=0;function Po(e,t){const n=vl(e,t);if(-1!==n)return n;const r=t[1];r.firstCreatePass&&(e.injectorIndex=t.length,Cl(r.data,e),Cl(t,null),Cl(r.blueprint,null));const o=ds(e,t),i=e.injectorIndex;if(Lf(o)){const s=Ir(o),a=Mr(o,t),l=a[1].data;for(let c=0;c<8;c++)t[i+c]=a[s+c]|l[s+c]}return t[i+8]=o,i}function Cl(e,t){e.push(0,0,0,0,0,0,0,0,t)}function vl(e,t){return-1===e.injectorIndex||e.parent&&e.parent.injectorIndex===e.injectorIndex||null===t[e.injectorIndex+8]?-1:e.injectorIndex}function ds(e,t){if(e.parent&&-1!==e.parent.injectorIndex)return e.parent.injectorIndex;let n=0,r=null,o=t;for(;null!==o;){const i=o[1],s=i.type;if(r=2===s?i.declTNode:1===s?o[6]:null,null===r)return-1;if(n++,o=o[15],-1!==r.injectorIndex)return r.injectorIndex|n<<16}return-1}function fs(e,t,n){!function(e,t,n){let r;"string"==typeof n?r=n.charCodeAt(0)||0:n.hasOwnProperty(Ao)&&(r=n[Ao]),null==r&&(r=n[Ao]=QD++);const o=255&r;t.data[e+(o>>5)]|=1<<o}(e,t,n)}function jf(e,t,n){if(n&O.Optional)return e;Wi(t,"NodeInjector")}function $f(e,t,n,r){if(n&O.Optional&&void 0===r&&(r=null),0==(n&(O.Self|O.Host))){const o=e[9],i=An(void 0);try{return o?o.get(t,r,n&O.Optional):lf(t,r,n&O.Optional)}finally{An(i)}}return jf(r,t,n)}function Uf(e,t,n,r=O.Default,o){if(null!==e){const i=function(e){if("string"==typeof e)return e.charCodeAt(0)||0;const t=e.hasOwnProperty(Ao)?e[Ao]:void 0;return"number"==typeof t?t>=0?255&t:ZD:t}(n);if("function"==typeof i){if(!Sf(t,e,r))return r&O.Host?jf(o,n,r):$f(t,n,r,o);try{const s=i(r);if(null!=s||r&O.Optional)return s;Wi(n)}finally{Ff()}}else if("number"==typeof i){let s=null,a=vl(e,t),l=-1,c=r&O.Host?t[16][6]:null;for((-1===a||r&O.SkipSelf)&&(l=-1===a?ds(e,t):t[a+8],-1!==l&&Wf(r,!1)?(s=t[1],a=Ir(l),t=Mr(l,t)):a=-1);-1!==a;){const u=t[1];if(zf(i,a,u.data)){const d=JD(a,t,n,s,r,c);if(d!==Gf)return d}l=t[a+8],-1!==l&&Wf(r,t[1].data[a+8]===c)&&zf(i,a,t)?(s=u,a=Ir(l),t=Mr(l,t)):a=-1}}}return $f(t,n,r,o)}const Gf={};function ZD(){return new Tr(xe(),b())}function JD(e,t,n,r,o,i){const s=t[1],a=s.data[e+8],u=function(e,t,n,r,o){const i=e.providerIndexes,s=t.data,a=1048575&i,l=e.directiveStart,u=i>>20,f=o?a+u:e.directiveEnd;for(let h=r?a:a+u;h<f;h++){const p=s[h];if(h<l&&n===p||h>=l&&p.type===n)return h}if(o){const h=s[l];if(h&&Vt(h)&&h.type===n)return l}return null}(a,s,n,null==r?Ji(a)&&yl:r!=s&&0!=(3&a.type),o&O.Host&&i===a);return null!==u?Vo(t,s,u,a):Gf}function Vo(e,t,n,r){let o=e[n];const i=t.data;if(function(e){return e instanceof Fo}(o)){const s=o;s.resolving&&function(e,t){throw new Qn("200",`Circular dependency in DI detected for ${e}`)}(Qe(i[n]));const a=us(s.canSeeViewProviders);s.resolving=!0;const l=s.injectImpl?An(s.injectImpl):null;Sf(e,r,O.Default);try{o=e[n]=s.factory(void 0,i,e,r),t.firstCreatePass&&n>=r.directiveStart&&function(e,t,n){const{ngOnChanges:r,ngOnInit:o,ngDoCheck:i}=t.type.prototype;if(r){const s=mf(t);(n.preOrderHooks||(n.preOrderHooks=[])).push(e,s),(n.preOrderCheckHooks||(n.preOrderCheckHooks=[])).push(e,s)}o&&(n.preOrderHooks||(n.preOrderHooks=[])).push(0-e,o),i&&((n.preOrderHooks||(n.preOrderHooks=[])).push(e,i),(n.preOrderCheckHooks||(n.preOrderCheckHooks=[])).push(e,i))}(n,i[n],t)}finally{null!==l&&An(l),us(a),s.resolving=!1,Ff()}}return o}function zf(e,t,n){return!!(n[t+(e>>5)]&1<<e)}function Wf(e,t){return!(e&O.Self||e&O.Host&&t)}class Tr{constructor(t,n){this._tNode=t,this._lView=n}get(t,n){return Uf(this._tNode,this._lView,t,void 0,n)}}function Et(e){return Sn(()=>{const t=e.prototype.constructor,n=t[gn]||Dl(t),r=Object.prototype;let o=Object.getPrototypeOf(e.prototype).constructor;for(;o&&o!==r;){const i=o[gn]||Dl(o);if(i&&i!==n)return i;o=Object.getPrototypeOf(o)}return i=>new i})}function Dl(e){return rf(e)?()=>{const t=Dl(N(e));return t&&t()}:Xn(e)}const Sr="__parameters__";function er(e,t,n){return Sn(()=>{const r=function(e){return function(...n){if(e){const r=e(...n);for(const o in r)this[o]=r[o]}}}(t);function o(...i){if(this instanceof o)return r.apply(this,i),this;const s=new o(...i);return a.annotation=s,a;function a(l,c,u){const d=l.hasOwnProperty(Sr)?l[Sr]:Object.defineProperty(l,Sr,{value:[]})[Sr];for(;d.length<=u;)d.push(null);return(d[u]=d[u]||[]).push(s),l}}return n&&(o.prototype=Object.create(n.prototype)),o.prototype.ngMetadataName=e,o.annotationCls=o,o})}class X{constructor(t,n){this._desc=t,this.ngMetadataName="InjectionToken",this.\u0275prov=void 0,"number"==typeof n?this.__NG_ELEMENT_ID__=n:void 0!==n&&(this.\u0275prov=te({token:this,providedIn:n.providedIn||"root",factory:n.factory}))}toString(){return`InjectionToken ${this._desc}`}}function Xt(e,t){e.forEach(n=>Array.isArray(n)?Xt(n,t):t(n))}function gs(e,t,n){t>=e.length?e.push(n):e.splice(t,0,n)}function tr(e,t){return t>=e.length-1?e.pop():e.splice(t,1)[0]}function pt(e,t,n){let r=Nr(e,t);return r>=0?e[1|r]=n:(r=~r,function(e,t,n,r){let o=e.length;if(o==t)e.push(n,r);else if(1===o)e.push(r,e[0]),e[0]=n;else{for(o--,e.push(e[o-1],e[o]);o>t;)e[o]=e[o-2],o--;e[t]=n,e[t+1]=r}}(e,r,t,n)),r}function Il(e,t){const n=Nr(e,t);if(n>=0)return e[1|n]}function Nr(e,t){return function(e,t,n){let r=0,o=e.length>>n;for(;o!==r;){const i=r+(o-r>>1),s=e[i<<n];if(t===s)return i<<n;s>t?o=i:r=i+1}return~(o<<n)}(e,t,1)}const Ho={},Tl="__NG_DI_FLAG__",Rr="ngTempTokenPath",db=/\n/gm,Al="__source",Sl=ee({provide:String,useValue:ee});let jo;function Fr(e){const t=jo;return jo=e,t}function hb(e,t=O.Default){if(void 0===jo)throw new Error("inject() must be called from an injection context");return null===jo?lf(e,void 0,t):jo.get(e,t&O.Optional?null:void 0,t)}function Y(e,t=O.Default){return(Za||hb)(N(e),t)}function nr(e){const t=[];for(let n=0;n<e.length;n++){const r=N(e[n]);if(Array.isArray(r)){if(0===r.length)throw new Error("Arguments array must have arguments.");let o,i=O.Default;for(let s=0;s<r.length;s++){const a=r[s],l=pb(a);"number"==typeof l?-1===l?o=a.token:i|=l:o=a}t.push(Y(o,i))}else t.push(Y(r))}return t}function $o(e,t){return e[Tl]=t,e.prototype[Tl]=t,e}function pb(e){return e[Tl]}function Jf(e,t,n,r){const o=e[Rr];throw t[Al]&&o.unshift(t[Al]),e.message=function(e,t,n,r=null){e=e&&"\n"===e.charAt(0)&&"\u0275"==e.charAt(1)?e.substr(2):e;let o=W(t);if(Array.isArray(t))o=t.map(W).join(" -> ");else if("object"==typeof t){let i=[];for(let s in t)if(t.hasOwnProperty(s)){let a=t[s];i.push(s+":"+("string"==typeof a?JSON.stringify(a):W(a)))}o=`{${i.join(", ")}}`}return`${n}${r?"("+r+")":""}[${o}]: ${e.replace(db,"\n  ")}`}("\n"+e.message,o,n,r),e.ngTokenPath=o,e[Rr]=null,e}const Uo=$o(er("Inject",e=>({token:e})),-1),en=$o(er("Optional"),8),rr=$o(er("SkipSelf"),4);class or{constructor(t){this.changingThisBreaksApplicationSecurity=t}toString(){return`SafeValue must use [property]=binding: ${this.changingThisBreaksApplicationSecurity} (see https://g.co/ng/security#xss)`}}function gt(e){return e instanceof or?e.changingThisBreaksApplicationSecurity:e}function tn(e,t){const n=function(e){return e instanceof or&&e.getTypeName()||null}(e);if(null!=n&&n!==t){if("ResourceURL"===n&&"URL"===t)return!0;throw new Error(`Required a safe ${t}, got a ${n} (see https://g.co/ng/security#xss)`)}return n===t}const Lb=/^(?:(?:https?|mailto|ftp|tel|file|sms):|[^&:/?#]*(?:[/?#]|$))/gi,Bb=/^data:(?:image\/(?:bmp|gif|jpeg|jpg|png|tiff|webp)|video\/(?:mpeg|mp4|ogg|webm)|audio\/(?:mp3|oga|ogg|opus));base64,[a-z0-9+\/]+=*$/i;var ce=(()=>((ce=ce||{})[ce.NONE=0]="NONE",ce[ce.HTML=1]="HTML",ce[ce.STYLE=2]="STYLE",ce[ce.SCRIPT=3]="SCRIPT",ce[ce.URL=4]="URL",ce[ce.RESOURCE_URL=5]="RESOURCE_URL",ce))();function Vr(e){const t=function(){const e=b();return e&&e[12]}();return t?t.sanitize(ce.URL,e)||"":tn(e,"URL")?gt(e):function(e){return(e=String(e)).match(Lb)||e.match(Bb)?e:"unsafe:"+e}(U(e))}const _h="__ngContext__";function He(e,t){e[_h]=t}function Ll(e){const t=function(e){return e[_h]||null}(e);return t?Array.isArray(t)?t:t.lView:null}function bs(e){return e.ngOriginalError}function aE(e,...t){e.error(...t)}class ir{constructor(){this._console=console}handleError(t){const n=this._findOriginalError(t),r=this._findContext(t),o=function(e){return e&&e.ngErrorLogger||aE}(t);o(this._console,"ERROR",t),n&&o(this._console,"ORIGINAL ERROR",n),r&&o(this._console,"ERROR CONTEXT",r)}_findContext(t){return t?function(e){return e.ngDebugContext}(t)||this._findContext(bs(t)):null}_findOriginalError(t){let n=t&&bs(t);for(;n&&bs(n);)n=bs(n);return n||null}}const Mh=(()=>("undefined"!=typeof requestAnimationFrame&&requestAnimationFrame||setTimeout).bind(ne))();function Hl(e){return e.ownerDocument.defaultView}function rn(e){return e instanceof Function?e():e}var mt=(()=>((mt=mt||{})[mt.Important=1]="Important",mt[mt.DashCase=2]="DashCase",mt))();function $l(e,t){return undefined(e,t)}function Ko(e){const t=e[3];return Pt(t)?t[3]:t}function Ul(e){return Nh(e[13])}function Gl(e){return Nh(e[4])}function Nh(e){for(;null!==e&&!Pt(e);)e=e[4];return e}function Lr(e,t,n,r,o){if(null!=r){let i,s=!1;Pt(r)?i=r:Yt(r)&&(s=!0,r=r[0]);const a=Me(r);0===e&&null!==n?null==o?kh(t,n,a):sr(t,n,a,o||null,!0):1===e&&null!==n?sr(t,n,a,o||null,!0):2===e?function(e,t,n){const r=ws(e,t);r&&function(e,t,n,r){ve(e)?e.removeChild(t,n,r):t.removeChild(n)}(e,r,t,n)}(t,a,s):3===e&&t.destroyNode(a),null!=i&&function(e,t,n,r,o){const i=n[7];i!==Me(n)&&Lr(t,e,r,i,o);for(let a=10;a<n.length;a++){const l=n[a];Yo(l[1],l,e,t,r,i)}}(t,e,i,n,o)}}function Wl(e,t,n){return ve(e)?e.createElement(t,n):null===n?e.createElement(t):e.createElementNS(n,t)}function Fh(e,t){const n=e[9],r=n.indexOf(t),o=t[3];1024&t[2]&&(t[2]&=-1025,cl(o,-1)),n.splice(r,1)}function ql(e,t){if(e.length<=10)return;const n=10+t,r=e[n];if(r){const o=r[17];null!==o&&o!==e&&Fh(o,r),t>0&&(e[n-1][4]=r[4]);const i=tr(e,10+t);!function(e,t){Yo(e,t,t[G],2,null,null),t[0]=null,t[6]=null}(r[1],r);const s=i[19];null!==s&&s.detachView(i[1]),r[3]=null,r[4]=null,r[2]&=-129}return r}function Oh(e,t){if(!(256&t[2])){const n=t[G];ve(n)&&n.destroyNode&&Yo(e,t,n,3,null,null),function(e){let t=e[13];if(!t)return Ql(e[1],e);for(;t;){let n=null;if(Yt(t))n=t[13];else{const r=t[10];r&&(n=r)}if(!n){for(;t&&!t[4]&&t!==e;)Yt(t)&&Ql(t[1],t),t=t[3];null===t&&(t=e),Yt(t)&&Ql(t[1],t),n=t&&t[4]}t=n}}(t)}}function Ql(e,t){if(!(256&t[2])){t[2]&=-129,t[2]|=256,function(e,t){let n;if(null!=e&&null!=(n=e.destroyHooks))for(let r=0;r<n.length;r+=2){const o=t[n[r]];if(!(o instanceof Fo)){const i=n[r+1];if(Array.isArray(i))for(let s=0;s<i.length;s+=2){const a=o[i[s]],l=i[s+1];try{l.call(a)}finally{}}else try{i.call(o)}finally{}}}}(e,t),function(e,t){const n=e.cleanup,r=t[7];let o=-1;if(null!==n)for(let i=0;i<n.length-1;i+=2)if("string"==typeof n[i]){const s=n[i+1],a="function"==typeof s?s(t):Me(t[s]),l=r[o=n[i+2]],c=n[i+3];"boolean"==typeof c?a.removeEventListener(n[i],l,c):c>=0?r[o=c]():r[o=-c].unsubscribe(),i+=2}else{const s=r[o=n[i+1]];n[i].call(s)}if(null!==r){for(let i=o+1;i<r.length;i++)r[i]();t[7]=null}}(e,t),1===t[1].type&&ve(t[G])&&t[G].destroy();const n=t[17];if(null!==n&&Pt(t[3])){n!==t[3]&&Fh(n,t);const r=t[19];null!==r&&r.detachView(e)}}}function Ph(e,t,n){return function(e,t,n){let r=t;for(;null!==r&&40&r.type;)r=(t=r).parent;if(null===r)return n[0];if(2&r.flags){const o=e.data[r.directiveStart].encapsulation;if(o===Se.None||o===Se.Emulated)return null}return bt(r,n)}(e,t.parent,n)}function sr(e,t,n,r,o){ve(e)?e.insertBefore(t,n,r,o):t.insertBefore(n,r,o)}function kh(e,t,n){ve(e)?e.appendChild(t,n):t.appendChild(n)}function Lh(e,t,n,r,o){null!==r?sr(e,t,n,r,o):kh(e,t,n)}function ws(e,t){return ve(e)?e.parentNode(t):t.parentNode}let jh=function(e,t,n){return 40&e.type?bt(e,n):null};function Is(e,t,n,r){const o=Ph(e,r,t),i=t[G],a=function(e,t,n){return jh(e,t,n)}(r.parent||t[6],r,t);if(null!=o)if(Array.isArray(n))for(let l=0;l<n.length;l++)Lh(i,o,n[l],a,!1);else Lh(i,o,n,a,!1)}function Ms(e,t){if(null!==t){const n=t.type;if(3&n)return bt(t,e);if(4&n)return Yl(-1,e[t.index]);if(8&n){const r=t.child;if(null!==r)return Ms(e,r);{const o=e[t.index];return Pt(o)?Yl(-1,o):Me(o)}}if(32&n)return $l(t,e)()||Me(e[t.index]);{const r=Uh(e,t);return null!==r?Array.isArray(r)?r[0]:Ms(Ko(e[16]),r):Ms(e,t.next)}}return null}function Uh(e,t){return null!==t?e[16][6].projection[t.projection]:null}function Yl(e,t){const n=10+e+1;if(n<t.length){const r=t[n],o=r[1].firstChild;if(null!==o)return Ms(r,o)}return t[7]}function Zl(e,t,n,r,o,i,s){for(;null!=n;){const a=r[n.index],l=n.type;if(s&&0===t&&(a&&He(Me(a),r),n.flags|=4),64!=(64&n.flags))if(8&l)Zl(e,t,n.child,r,o,i,!1),Lr(t,e,o,a,i);else if(32&l){const c=$l(n,r);let u;for(;u=c();)Lr(t,e,o,u,i);Lr(t,e,o,a,i)}else 16&l?zh(e,t,r,n,o,i):Lr(t,e,o,a,i);n=s?n.projectionNext:n.next}}function Yo(e,t,n,r,o,i){Zl(n,r,e.firstChild,t,o,i,!1)}function zh(e,t,n,r,o,i){const s=n[16],l=s[6].projection[r.projection];if(Array.isArray(l))for(let c=0;c<l.length;c++)Lr(t,e,o,l[c],i);else Zl(e,t,l,s[3],o,i,!0)}function Wh(e,t,n){ve(e)?e.setAttribute(t,"style",n):t.style.cssText=n}function Jl(e,t,n){ve(e)?""===n?e.removeAttribute(t,"class"):e.setAttribute(t,"class",n):t.className=n}function qh(e,t,n){let r=e.length;for(;;){const o=e.indexOf(t,n);if(-1===o)return o;if(0===o||e.charCodeAt(o-1)<=32){const i=t.length;if(o+i===r||e.charCodeAt(o+i)<=32)return o}n=o+1}}const Qh="ng-template";function RE(e,t,n){let r=0;for(;r<e.length;){let o=e[r++];if(n&&"class"===o){if(o=e[r],-1!==qh(o.toLowerCase(),t,0))return!0}else if(1===o){for(;r<e.length&&"string"==typeof(o=e[r++]);)if(o.toLowerCase()===t)return!0;return!1}}return!1}function Kh(e){return 4===e.type&&e.value!==Qh}function FE(e,t,n){return t===(4!==e.type||n?e.value:Qh)}function OE(e,t,n){let r=4;const o=e.attrs||[],i=function(e){for(let t=0;t<e.length;t++)if(Vf(e[t]))return t;return e.length}(o);let s=!1;for(let a=0;a<t.length;a++){const l=t[a];if("number"!=typeof l){if(!s)if(4&r){if(r=2|1&r,""!==l&&!FE(e,l,n)||""===l&&1===t.length){if(kt(r))return!1;s=!0}}else{const c=8&r?l:t[++a];if(8&r&&null!==e.attrs){if(!RE(e.attrs,c,n)){if(kt(r))return!1;s=!0}continue}const d=PE(8&r?"class":l,o,Kh(e),n);if(-1===d){if(kt(r))return!1;s=!0;continue}if(""!==c){let f;f=d>i?"":o[d+1].toLowerCase();const h=8&r?f:null;if(h&&-1!==qh(h,c,0)||2&r&&c!==f){if(kt(r))return!1;s=!0}}}}else{if(!s&&!kt(r)&&!kt(l))return!1;if(s&&kt(l))continue;s=!1,r=l|1&r}}return kt(r)||s}function kt(e){return 0==(1&e)}function PE(e,t,n,r){if(null===t)return-1;let o=0;if(r||!n){let i=!1;for(;o<t.length;){const s=t[o];if(s===e)return o;if(3===s||6===s)i=!0;else{if(1===s||2===s){let a=t[++o];for(;"string"==typeof a;)a=t[++o];continue}if(4===s)break;if(0===s){o+=4;continue}}o+=i?1:2}return-1}return function(e,t){let n=e.indexOf(4);if(n>-1)for(n++;n<e.length;){const r=e[n];if("number"==typeof r)return-1;if(r===t)return n;n++}return-1}(t,e)}function Yh(e,t,n=!1){for(let r=0;r<t.length;r++)if(OE(e,t[r],n))return!0;return!1}function Zh(e,t){return e?":not("+t.trim()+")":t}function HE(e){let t=e[0],n=1,r=2,o="",i=!1;for(;n<e.length;){let s=e[n];if("string"==typeof s)if(2&r){const a=e[++n];o+="["+s+(a.length>0?'="'+a+'"':"")+"]"}else 8&r?o+="."+s:4&r&&(o+=" "+s);else""!==o&&!kt(s)&&(t+=Zh(i,o),o=""),r=s,i=i||!kt(r);n++}return""!==o&&(t+=Zh(i,o)),t}const j={};function g(e){Jh(J(),b(),Ze()+e,ts())}function Jh(e,t,n,r){if(!r)if(3==(3&t[2])){const i=e.preOrderCheckHooks;null!==i&&ss(t,i,n)}else{const i=e.preOrderHooks;null!==i&&as(t,i,0,n)}Fn(n)}function Ts(e,t){return e<<17|t<<2}function Lt(e){return e>>17&32767}function Xl(e){return 2|e}function yn(e){return(131068&e)>>2}function ec(e,t){return-131069&e|t<<2}function tc(e){return 1|e}function lp(e,t){const n=e.contentQueries;if(null!==n)for(let r=0;r<n.length;r+=2){const o=n[r],i=n[r+1];if(-1!==i){const s=e.data[i];pl(o),s.contentQueries(2,t[i],i)}}}function Zo(e,t,n,r,o,i,s,a,l,c){const u=t.blueprint.slice();return u[0]=o,u[2]=140|r,Ef(u),u[3]=u[15]=e,u[8]=n,u[10]=s||e&&e[10],u[G]=a||e&&e[G],u[12]=l||e&&e[12]||null,u[9]=c||e&&e[9]||null,u[6]=i,u[16]=2==t.type?e[16]:u,u}function Br(e,t,n,r,o){let i=e.data[t];if(null===i)i=function(e,t,n,r,o){const i=If(),s=ul(),l=e.data[t]=function(e,t,n,r,o,i){return{type:n,index:r,insertBeforeIndex:null,injectorIndex:t?t.injectorIndex:-1,directiveStart:-1,directiveEnd:-1,directiveStylingLast:-1,propertyBindings:null,flags:0,providerIndexes:0,value:o,attrs:i,mergedAttrs:null,localNames:null,initialInputs:void 0,inputs:null,outputs:null,tViews:null,next:null,projectionNext:null,child:null,parent:t,projection:null,styles:null,stylesWithoutHost:null,residualStyles:void 0,classes:null,classesWithoutHost:null,residualClasses:void 0,classBindings:0,styleBindings:0}}(0,s?i:i&&i.parent,n,t,r,o);return null===e.firstChild&&(e.firstChild=l),null!==i&&(s?null==i.child&&null!==l.parent&&(i.child=l):null===i.next&&(i.next=l)),l}(e,t,n,r,o),B.lFrame.inI18n&&(i.flags|=64);else if(64&i.type){i.type=n,i.value=r,i.attrs=o;const s=function(){const e=B.lFrame,t=e.currentTNode;return e.isParent?t:t.parent}();i.injectorIndex=null===s?-1:s.injectorIndex}return Zt(i,!0),i}function Hr(e,t,n,r){if(0===n)return-1;const o=t.length;for(let i=0;i<n;i++)t.push(r),e.blueprint.push(r),e.data.push(null);return o}function Jo(e,t,n){rs(t);try{const r=e.viewQuery;null!==r&&_c(1,r,n);const o=e.template;null!==o&&cp(e,t,o,1,n),e.firstCreatePass&&(e.firstCreatePass=!1),e.staticContentQueries&&lp(e,t),e.staticViewQueries&&_c(2,e.viewQuery,n);const i=e.components;null!==i&&function(e,t){for(let n=0;n<t.length;n++)Cw(e,t[n])}(t,i)}catch(r){throw e.firstCreatePass&&(e.incompleteFirstPass=!0,e.firstCreatePass=!1),r}finally{t[2]&=-5,os()}}function jr(e,t,n,r){const o=t[2];if(256==(256&o))return;rs(t);const i=ts();try{Ef(t),function(e){B.lFrame.bindingIndex=e}(e.bindingStartIndex),null!==n&&cp(e,t,n,2,r);const s=3==(3&o);if(!i)if(s){const c=e.preOrderCheckHooks;null!==c&&ss(t,c,null)}else{const c=e.preOrderHooks;null!==c&&as(t,c,0,null),gl(t,0)}if(function(e){for(let t=Ul(e);null!==t;t=Gl(t)){if(!t[2])continue;const n=t[9];for(let r=0;r<n.length;r++){const o=n[r],i=o[3];0==(1024&o[2])&&cl(i,1),o[2]|=1024}}}(t),function(e){for(let t=Ul(e);null!==t;t=Gl(t))for(let n=10;n<t.length;n++){const r=t[n],o=r[1];ll(r)&&jr(o,r,o.template,r[8])}}(t),null!==e.contentQueries&&lp(e,t),!i)if(s){const c=e.contentCheckHooks;null!==c&&ss(t,c)}else{const c=e.contentHooks;null!==c&&as(t,c,1),gl(t,1)}!function(e,t){const n=e.hostBindingOpCodes;if(null!==n)try{for(let r=0;r<n.length;r++){const o=n[r];if(o<0)Fn(~o);else{const i=o,s=n[++r],a=n[++r];RD(s,i),a(2,t[i])}}}finally{Fn(-1)}}(e,t);const a=e.components;null!==a&&function(e,t){for(let n=0;n<t.length;n++)yw(e,t[n])}(t,a);const l=e.viewQuery;if(null!==l&&_c(2,l,r),!i)if(s){const c=e.viewCheckHooks;null!==c&&ss(t,c)}else{const c=e.viewHooks;null!==c&&as(t,c,2),gl(t,2)}!0===e.firstUpdatePass&&(e.firstUpdatePass=!1),i||(t[2]&=-73),1024&t[2]&&(t[2]&=-1025,cl(t[3],-1))}finally{os()}}function XE(e,t,n,r){const o=t[10],i=!ts(),s=function(e){return 4==(4&e[2])}(t);try{i&&!s&&o.begin&&o.begin(),s&&Jo(e,t,r),jr(e,t,n,r)}finally{i&&!s&&o.end&&o.end()}}function cp(e,t,n,r,o){const i=Ze(),s=2&r;try{Fn(-1),s&&t.length>20&&Jh(e,t,20,ts()),n(r,o)}finally{Fn(i)}}function up(e,t,n){if(nl(t)){const o=t.directiveEnd;for(let i=t.directiveStart;i<o;i++){const s=e.data[i];s.contentQueries&&s.contentQueries(1,n[i],i)}}}function cc(e,t,n){!wf()||(function(e,t,n,r){const o=n.directiveStart,i=n.directiveEnd;e.firstCreatePass||Po(n,t),He(r,t);const s=n.initialInputs;for(let a=o;a<i;a++){const l=e.data[a],c=Vt(l);c&&hw(t,n,l);const u=Vo(t,e,a,n);He(u,t),null!==s&&pw(0,a-o,u,l,0,s),c&&(ht(n.index,t)[8]=u)}}(e,t,n,bt(n,t)),128==(128&n.flags)&&function(e,t,n){const r=n.directiveStart,o=n.directiveEnd,s=n.index,a=B.lFrame.currentDirectiveIndex;try{Fn(s);for(let l=r;l<o;l++){const c=e.data[l],u=t[l];fl(l),(null!==c.hostBindings||0!==c.hostVars||null!==c.hostAttrs)&&yp(c,u)}}finally{Fn(-1),fl(a)}}(e,t,n))}function uc(e,t,n=bt){const r=t.localNames;if(null!==r){let o=t.index+1;for(let i=0;i<r.length;i+=2){const s=r[i+1],a=-1===s?n(t,e):e[s];e[o++]=a}}}function dp(e){const t=e.tView;return null===t||t.incompleteFirstPass?e.tView=xs(1,null,e.template,e.decls,e.vars,e.directiveDefs,e.pipeDefs,e.viewQuery,e.schemas,e.consts):t}function xs(e,t,n,r,o,i,s,a,l,c){const u=20+r,d=u+o,f=function(e,t){const n=[];for(let r=0;r<t;r++)n.push(r<e?null:j);return n}(u,d),h="function"==typeof c?c():c;return f[1]={type:e,blueprint:f,template:n,queries:null,viewQuery:a,declTNode:t,data:f.slice().fill(null,u),bindingStartIndex:u,expandoStartIndex:d,hostBindingOpCodes:null,firstCreatePass:!0,firstUpdatePass:!0,staticViewQueries:!1,staticContentQueries:!1,preOrderHooks:null,preOrderCheckHooks:null,contentHooks:null,contentCheckHooks:null,viewHooks:null,viewCheckHooks:null,destroyHooks:null,cleanup:null,contentQueries:null,components:null,directiveRegistry:"function"==typeof i?i():i,pipeRegistry:"function"==typeof s?s():s,firstChild:null,schemas:l,consts:h,incompleteFirstPass:!1}}function gp(e,t,n){for(let r in e)if(e.hasOwnProperty(r)){const o=e[r];(n=null===n?{}:n).hasOwnProperty(r)?n[r].push(t,o):n[r]=[t,o]}return n}function _t(e,t,n,r,o,i,s,a){const l=bt(t,n);let u,c=t.inputs;!a&&null!=c&&(u=c[r])?(Ap(e,n,u,r,o),Ji(t)&&function(e,t){const n=ht(t,e);16&n[2]||(n[2]|=64)}(n,t.index)):3&t.type&&(r=function(e){return"class"===e?"className":"for"===e?"htmlFor":"formaction"===e?"formAction":"innerHtml"===e?"innerHTML":"readonly"===e?"readOnly":"tabindex"===e?"tabIndex":e}(r),o=null!=s?s(o,t.value||"",r):o,ve(i)?i.setProperty(l,r,o):_l(r)||(l.setProperty?l.setProperty(r,o):l[r]=o))}function dc(e,t,n,r){let o=!1;if(wf()){const i=function(e,t,n){const r=e.directiveRegistry;let o=null;if(r)for(let i=0;i<r.length;i++){const s=r[i];Yh(n,s.selectors,!1)&&(o||(o=[]),fs(Po(n,t),e,s.type),Vt(s)?(Cp(e,n),o.unshift(s)):o.push(s))}return o}(e,t,n),s=null===r?null:{"":-1};if(null!==i){o=!0,vp(n,e.data.length,i.length);for(let u=0;u<i.length;u++){const d=i[u];d.providersResolver&&d.providersResolver(d)}let a=!1,l=!1,c=Hr(e,t,i.length,null);for(let u=0;u<i.length;u++){const d=i[u];n.mergedAttrs=cs(n.mergedAttrs,d.hostAttrs),Dp(e,n,t,c,d),fw(c,d,s),null!==d.contentQueries&&(n.flags|=8),(null!==d.hostBindings||null!==d.hostAttrs||0!==d.hostVars)&&(n.flags|=128);const f=d.type.prototype;!a&&(f.ngOnChanges||f.ngOnInit||f.ngDoCheck)&&((e.preOrderHooks||(e.preOrderHooks=[])).push(n.index),a=!0),!l&&(f.ngOnChanges||f.ngDoCheck)&&((e.preOrderCheckHooks||(e.preOrderCheckHooks=[])).push(n.index),l=!0),c++}!function(e,t){const r=t.directiveEnd,o=e.data,i=t.attrs,s=[];let a=null,l=null;for(let c=t.directiveStart;c<r;c++){const u=o[c],d=u.inputs,f=null===i||Kh(t)?null:gw(d,i);s.push(f),a=gp(d,c,a),l=gp(u.outputs,c,l)}null!==a&&(a.hasOwnProperty("class")&&(t.flags|=16),a.hasOwnProperty("style")&&(t.flags|=32)),t.initialInputs=s,t.inputs=a,t.outputs=l}(e,n)}s&&function(e,t,n){if(t){const r=e.localNames=[];for(let o=0;o<t.length;o+=2){const i=n[t[o+1]];if(null==i)throw new Qn("301",`Export of name '${t[o+1]}' not found!`);r.push(t[o],i)}}}(n,r,s)}return n.mergedAttrs=cs(n.mergedAttrs,n.attrs),o}function _p(e,t,n,r,o,i){const s=i.hostBindings;if(s){let a=e.hostBindingOpCodes;null===a&&(a=e.hostBindingOpCodes=[]);const l=~t.index;(function(e){let t=e.length;for(;t>0;){const n=e[--t];if("number"==typeof n&&n<0)return n}return 0})(a)!=l&&a.push(l),a.push(r,o,s)}}function yp(e,t){null!==e.hostBindings&&e.hostBindings(1,t)}function Cp(e,t){t.flags|=2,(e.components||(e.components=[])).push(t.index)}function fw(e,t,n){if(n){if(t.exportAs)for(let r=0;r<t.exportAs.length;r++)n[t.exportAs[r]]=e;Vt(t)&&(n[""]=e)}}function vp(e,t,n){e.flags|=1,e.directiveStart=t,e.directiveEnd=t+n,e.providerIndexes=t}function Dp(e,t,n,r,o){e.data[r]=o;const i=o.factory||(o.factory=Xn(o.type)),s=new Fo(i,Vt(o),null);e.blueprint[r]=s,n[r]=s,_p(e,t,0,r,Hr(e,n,o.hostVars,j),o)}function hw(e,t,n){const r=bt(t,e),o=dp(n),i=e[10],s=Ns(e,Zo(e,o,null,n.onPush?64:16,r,t,i,i.createRenderer(r,n),null,null));e[t.index]=s}function on(e,t,n,r,o,i){const s=bt(e,t);!function(e,t,n,r,o,i,s){if(null==i)ve(e)?e.removeAttribute(t,o,n):t.removeAttribute(o);else{const a=null==s?U(i):s(i,r||"",o);ve(e)?e.setAttribute(t,o,a,n):n?t.setAttributeNS(n,o,a):t.setAttribute(o,a)}}(t[G],s,i,e.value,n,r,o)}function pw(e,t,n,r,o,i){const s=i[t];if(null!==s){const a=r.setInput;for(let l=0;l<s.length;){const c=s[l++],u=s[l++],d=s[l++];null!==a?r.setInput(n,d,c,u):n[u]=d}}}function gw(e,t){let n=null,r=0;for(;r<t.length;){const o=t[r];if(0!==o)if(5!==o){if("number"==typeof o)break;e.hasOwnProperty(o)&&(null===n&&(n=[]),n.push(o,e[o],t[r+1])),r+=2}else r+=2;else r+=4}return n}function bp(e,t,n,r){return new Array(e,!0,!1,t,null,0,r,n,null,null)}function yw(e,t){const n=ht(t,e);if(ll(n)){const r=n[1];80&n[2]?jr(r,n,r.template,n[8]):n[5]>0&&hc(n)}}function hc(e){for(let r=Ul(e);null!==r;r=Gl(r))for(let o=10;o<r.length;o++){const i=r[o];if(1024&i[2]){const s=i[1];jr(s,i,s.template,i[8])}else i[5]>0&&hc(i)}const n=e[1].components;if(null!==n)for(let r=0;r<n.length;r++){const o=ht(n[r],e);ll(o)&&o[5]>0&&hc(o)}}function Cw(e,t){const n=ht(t,e),r=n[1];(function(e,t){for(let n=t.length;n<e.blueprint.length;n++)t.push(e.blueprint[n])})(r,n),Jo(r,n,n[8])}function Ns(e,t){return e[13]?e[14][4]=t:e[13]=t,e[14]=t,t}function pc(e){for(;e;){e[2]|=64;const t=Ko(e);if(hD(e)&&!t)return e;e=t}return null}function mc(e,t,n){const r=t[10];r.begin&&r.begin();try{jr(e,t,e.template,n)}catch(o){throw Tp(t,o),o}finally{r.end&&r.end()}}function Ep(e){!function(e){for(let t=0;t<e.components.length;t++){const n=e.components[t],r=Ll(n),o=r[1];XE(o,r,o.template,n)}}(e[8])}function _c(e,t,n){pl(0),t(e,n)}const ww=(()=>Promise.resolve(null))();function wp(e){return e[7]||(e[7]=[])}function Ip(e){return e.cleanup||(e.cleanup=[])}function Tp(e,t){const n=e[9],r=n?n.get(ir,null):null;r&&r.handleError(t)}function Ap(e,t,n,r,o){for(let i=0;i<n.length;){const s=n[i++],a=n[i++],l=t[s],c=e.data[s];null!==c.setInput?c.setInput(l,o,r,a):l[a]=o}}function vn(e,t,n){const r=es(t,e);!function(e,t,n){ve(e)?e.setValue(t,n):t.textContent=n}(e[G],r,n)}function Rs(e,t,n){let r=n?e.styles:null,o=n?e.classes:null,i=0;if(null!==t)for(let s=0;s<t.length;s++){const a=t[s];"number"==typeof a?i=a:1==i?o=Qa(o,a):2==i&&(r=Qa(r,a+": "+t[++s]+";"))}n?e.styles=r:e.stylesWithoutHost=r,n?e.classes=o:e.classesWithoutHost=o}const $r=new X("INJECTOR",-1);class Sp{get(t,n=Ho){if(n===Ho){const r=new Error(`NullInjectorError: No provider for ${W(t)}!`);throw r.name="NullInjectorError",r}return n}}const Xo=new X("Set Injector scope."),ei={},Tw={};let yc;function xp(){return void 0===yc&&(yc=new Sp),yc}function Np(e,t=null,n=null,r){return new Sw(e,n,t||xp(),r)}class Sw{constructor(t,n,r,o=null){this.parent=r,this.records=new Map,this.injectorDefTypes=new Set,this.onDestroy=new Set,this._destroyed=!1;const i=[];n&&Xt(n,a=>this.processProvider(a,t,n)),Xt([t],a=>this.processInjectorType(a,[],i)),this.records.set($r,Ur(void 0,this));const s=this.records.get(Xo);this.scope=null!=s?s.value:null,this.source=o||("object"==typeof t?null:W(t))}get destroyed(){return this._destroyed}destroy(){this.assertNotDestroyed(),this._destroyed=!0;try{this.onDestroy.forEach(t=>t.ngOnDestroy())}finally{this.records.clear(),this.onDestroy.clear(),this.injectorDefTypes.clear()}}get(t,n=Ho,r=O.Default){this.assertNotDestroyed();const o=Fr(this),i=An(void 0);try{if(!(r&O.SkipSelf)){let a=this.records.get(t);if(void 0===a){const l=function(e){return"function"==typeof e||"object"==typeof e&&e instanceof X}(t)&&pn(t);a=l&&this.injectableDefInScope(l)?Ur(Cc(t),ei):null,this.records.set(t,a)}if(null!=a)return this.hydrate(t,a)}return(r&O.Self?xp():this.parent).get(t,n=r&O.Optional&&n===Ho?null:n)}catch(s){if("NullInjectorError"===s.name){if((s[Rr]=s[Rr]||[]).unshift(W(t)),o)throw s;return Jf(s,t,"R3InjectorError",this.source)}throw s}finally{An(i),Fr(o)}}_resolveInjectorDefTypes(){this.injectorDefTypes.forEach(t=>this.get(t))}toString(){const t=[];return this.records.forEach((r,o)=>t.push(W(o))),`R3Injector[${t.join(", ")}]`}assertNotDestroyed(){if(this._destroyed)throw new Error("Injector has already been destroyed.")}processInjectorType(t,n,r){if(!(t=N(t)))return!1;let o=sf(t);const i=null==o&&t.ngModule||void 0,s=void 0===i?t:i,a=-1!==r.indexOf(s);if(void 0!==i&&(o=sf(i)),null==o)return!1;if(null!=o.imports&&!a){let u;r.push(s);try{Xt(o.imports,d=>{this.processInjectorType(d,n,r)&&(void 0===u&&(u=[]),u.push(d))})}finally{}if(void 0!==u)for(let d=0;d<u.length;d++){const{ngModule:f,providers:h}=u[d];Xt(h,p=>this.processProvider(p,f,h||ie))}}this.injectorDefTypes.add(s);const l=Xn(s)||(()=>new s);this.records.set(s,Ur(l,ei));const c=o.providers;if(null!=c&&!a){const u=t;Xt(c,d=>this.processProvider(d,u,c))}return void 0!==i&&void 0!==t.providers}processProvider(t,n,r){let o=Gr(t=N(t))?t:N(t&&t.provide);const i=function(e,t,n){return Fp(e)?Ur(void 0,e.useValue):Ur(Rp(e),ei)}(t);if(Gr(t)||!0!==t.multi)this.records.get(o);else{let s=this.records.get(o);s||(s=Ur(void 0,ei,!0),s.factory=()=>nr(s.multi),this.records.set(o,s)),o=t,s.multi.push(t)}this.records.set(o,i)}hydrate(t,n){return n.value===ei&&(n.value=Tw,n.value=n.factory()),"object"==typeof n.value&&n.value&&function(e){return null!==e&&"object"==typeof e&&"function"==typeof e.ngOnDestroy}(n.value)&&this.onDestroy.add(n.value),n.value}injectableDefInScope(t){if(!t.providedIn)return!1;const n=N(t.providedIn);return"string"==typeof n?"any"===n||n===this.scope:this.injectorDefTypes.has(n)}}function Cc(e){const t=pn(e),n=null!==t?t.factory:Xn(e);if(null!==n)return n;if(e instanceof X)throw new Error(`Token ${W(e)} is missing a \u0275prov definition.`);if(e instanceof Function)return function(e){const t=e.length;if(t>0){const r=function(e,t){const n=[];for(let r=0;r<e;r++)n.push(t);return n}(t,"?");throw new Error(`Can't resolve all parameters for ${W(e)}: (${r.join(", ")}).`)}const n=function(e){const t=e&&(e[qi]||e[af]);if(t){const n=function(e){if(e.hasOwnProperty("name"))return e.name;const t=(""+e).match(/^function\s*([^\s(]+)/);return null===t?"":t[1]}(e);return console.warn(`DEPRECATED: DI is instantiating a token "${n}" that inherits its @Injectable decorator but does not provide one itself.\nThis will become an error in a future version of Angular. Please add @Injectable() to the "${n}" class.`),t}return null}(e);return null!==n?()=>n.factory(e):()=>new e}(e);throw new Error("unreachable")}function Rp(e,t,n){let r;if(Gr(e)){const o=N(e);return Xn(o)||Cc(o)}if(Fp(e))r=()=>N(e.useValue);else if(function(e){return!(!e||!e.useFactory)}(e))r=()=>e.useFactory(...nr(e.deps||[]));else if(function(e){return!(!e||!e.useExisting)}(e))r=()=>Y(N(e.useExisting));else{const o=N(e&&(e.useClass||e.provide));if(!function(e){return!!e.deps}(e))return Xn(o)||Cc(o);r=()=>new o(...nr(e.deps))}return r}function Ur(e,t,n=!1){return{factory:e,value:t,multi:n?[]:void 0}}function Fp(e){return null!==e&&"object"==typeof e&&Sl in e}function Gr(e){return"function"==typeof e}const Op=function(e,t,n){return function(e,t=null,n=null,r){const o=Np(e,t,n,r);return o._resolveInjectorDefTypes(),o}({name:n},t,e,n)};let pe=(()=>{class e{static create(n,r){return Array.isArray(n)?Op(n,r,""):Op(n.providers,n.parent,n.name||"")}}return e.THROW_IF_NOT_FOUND=Ho,e.NULL=new Sp,e.\u0275prov=te({token:e,providedIn:"any",factory:()=>Y($r)}),e.__NG_ELEMENT_ID__=-1,e})();function Yw(e,t){is(Ll(e)[1],xe())}function ge(e){let t=function(e){return Object.getPrototypeOf(e.prototype).constructor}(e.type),n=!0;const r=[e];for(;t;){let o;if(Vt(e))o=t.\u0275cmp||t.\u0275dir;else{if(t.\u0275cmp)throw new Error("Directives cannot inherit Components");o=t.\u0275dir}if(o){if(n){r.push(o);const s=e;s.inputs=Ic(e.inputs),s.declaredInputs=Ic(e.declaredInputs),s.outputs=Ic(e.outputs);const a=o.hostBindings;a&&e0(e,a);const l=o.viewQuery,c=o.contentQueries;if(l&&Jw(e,l),c&&Xw(e,c),qa(e.inputs,o.inputs),qa(e.declaredInputs,o.declaredInputs),qa(e.outputs,o.outputs),Vt(o)&&o.data.animation){const u=e.data;u.animation=(u.animation||[]).concat(o.data.animation)}}const i=o.features;if(i)for(let s=0;s<i.length;s++){const a=i[s];a&&a.ngInherit&&a(e),a===ge&&(n=!1)}}t=Object.getPrototypeOf(t)}!function(e){let t=0,n=null;for(let r=e.length-1;r>=0;r--){const o=e[r];o.hostVars=t+=o.hostVars,o.hostAttrs=cs(o.hostAttrs,n=cs(n,o.hostAttrs))}}(r)}function Ic(e){return e===yr?{}:e===ie?[]:e}function Jw(e,t){const n=e.viewQuery;e.viewQuery=n?(r,o)=>{t(r,o),n(r,o)}:t}function Xw(e,t){const n=e.contentQueries;e.contentQueries=n?(r,o,i)=>{t(r,o,i),n(r,o,i)}:t}function e0(e,t){const n=e.hostBindings;e.hostBindings=n?(r,o)=>{t(r,o),n(r,o)}:t}let Fs=null;function zr(){if(!Fs){const e=ne.Symbol;if(e&&e.iterator)Fs=e.iterator;else{const t=Object.getOwnPropertyNames(Map.prototype);for(let n=0;n<t.length;++n){const r=t[n];"entries"!==r&&"size"!==r&&Map.prototype[r]===Map.prototype.entries&&(Fs=r)}}}return Fs}class Ht{constructor(t){this.wrapped=t}static wrap(t){return new Ht(t)}static unwrap(t){return Ht.isWrapped(t)?t.wrapped:t}static isWrapped(t){return t instanceof Ht}}function ni(e){return!!Mc(e)&&(Array.isArray(e)||!(e instanceof Map)&&zr()in e)}function Mc(e){return null!==e&&("function"==typeof e||"object"==typeof e)}function sn(e,t,n){return e[t]=n}function je(e,t,n){return!Object.is(e[t],n)&&(e[t]=n,!0)}function ar(e,t,n,r){const o=je(e,t,n);return je(e,t+1,r)||o}function an(e,t,n,r){const o=b();return je(o,wr(),t)&&(J(),on(De(),o,e,t,n,r)),an}function qr(e,t,n,r){return je(e,wr(),n)?t+U(n)+r:j}function S(e,t,n,r,o,i,s,a){const l=b(),c=J(),u=e+20,d=c.firstCreatePass?function(e,t,n,r,o,i,s,a,l){const c=t.consts,u=Br(t,e,4,s||null,Rn(c,a));dc(t,n,u,Rn(c,l)),is(t,u);const d=u.tViews=xs(2,u,r,o,i,t.directiveRegistry,t.pipeRegistry,null,t.schemas,c);return null!==t.queries&&(t.queries.template(t,u),d.queries=t.queries.embeddedTView(u)),u}(u,c,l,t,n,r,o,i,s):c.data[u];Zt(d,!1);const f=l[G].createComment("");Is(c,l,f,d),He(f,l),Ns(l,l[u]=bp(f,l,f,d)),Xi(d)&&cc(c,l,d),null!=s&&uc(l,d,a)}function I(e,t=O.Default){const n=b();return null===n?Y(e,t):Uf(xe(),n,N(e),t)}function D(e,t,n){const r=b();return je(r,wr(),t)&&_t(J(),De(),r,e,t,r[G],n,!1),D}function Nc(e,t,n,r,o){const s=o?"class":"style";Ap(e,n,t.inputs[s],s,r)}function y(e,t,n,r){const o=b(),i=J(),s=20+e,a=o[G],l=o[s]=Wl(a,t,B.lFrame.currentNamespace),c=i.firstCreatePass?function(e,t,n,r,o,i,s){const a=t.consts,c=Br(t,e,2,o,Rn(a,i));return dc(t,n,c,Rn(a,s)),null!==c.attrs&&Rs(c,c.attrs,!1),null!==c.mergedAttrs&&Rs(c,c.mergedAttrs,!0),null!==t.queries&&t.queries.elementStart(t,c),c}(s,i,o,0,t,n,r):i.data[s];Zt(c,!0);const u=c.mergedAttrs;null!==u&&ls(a,l,u);const d=c.classes;null!==d&&Jl(a,l,d);const f=c.styles;null!==f&&Wh(a,l,f),64!=(64&c.flags)&&Is(i,o,l,c),0===B.lFrame.elementDepthCount&&He(l,o),B.lFrame.elementDepthCount++,Xi(c)&&(cc(i,o,c),up(i,c,o)),null!==r&&uc(o,c)}function C(){let e=xe();ul()?dl():(e=e.parent,Zt(e,!1));const t=e;B.lFrame.elementDepthCount--;const n=J();n.firstCreatePass&&(is(n,e),nl(e)&&n.queries.elementEnd(e)),null!=t.classesWithoutHost&&function(e){return 0!=(16&e.flags)}(t)&&Nc(n,t,b(),t.classesWithoutHost,!0),null!=t.stylesWithoutHost&&function(e){return 0!=(32&e.flags)}(t)&&Nc(n,t,b(),t.stylesWithoutHost,!1)}function k(e,t,n,r){y(e,t,n,r),C()}function se(e,t,n){const r=b(),o=J(),i=e+20,s=o.firstCreatePass?function(e,t,n,r,o){const i=t.consts,s=Rn(i,r),a=Br(t,e,8,"ng-container",s);return null!==s&&Rs(a,s,!0),dc(t,n,a,Rn(i,o)),null!==t.queries&&t.queries.elementStart(t,a),a}(i,o,r,t,n):o.data[i];Zt(s,!0);const a=r[i]=r[G].createComment("");Is(o,r,a,s),He(a,r),Xi(s)&&(cc(o,r,s),up(o,s,r)),null!=n&&uc(r,s)}function ae(){let e=xe();const t=J();ul()?dl():(e=e.parent,Zt(e,!1)),t.firstCreatePass&&(is(t,e),nl(e)&&t.queries.elementEnd(e))}function ln(){return b()}function Vs(e){return!!e&&"function"==typeof e.then}const Rc=function(e){return!!e&&"function"==typeof e.subscribe};function Z(e,t,n,r){const o=b(),i=J(),s=xe();return function(e,t,n,r,o,i,s,a){const l=Xi(r),u=e.firstCreatePass&&Ip(e),d=t[8],f=wp(t);let h=!0;if(3&r.type||a){const m=bt(r,t),E=a?a(m):m,v=f.length,x=a?R=>a(Me(R[r.index])):r.index;if(ve(n)){let R=null;if(!a&&l&&(R=function(e,t,n,r){const o=e.cleanup;if(null!=o)for(let i=0;i<o.length-1;i+=2){const s=o[i];if(s===n&&o[i+1]===r){const a=t[7],l=o[i+2];return a.length>l?a[l]:null}"string"==typeof s&&(i+=2)}return null}(e,t,o,r.index)),null!==R)(R.__ngLastListenerFn__||R).__ngNextListenerFn__=i,R.__ngLastListenerFn__=i,h=!1;else{i=Fc(r,t,d,i,!1);const q=n.listen(E,o,i);f.push(i,q),u&&u.push(o,x,v,v+1)}}else i=Fc(r,t,d,i,!0),E.addEventListener(o,i,s),f.push(i),u&&u.push(o,x,v,s)}else i=Fc(r,t,d,i,!1);const p=r.outputs;let _;if(h&&null!==p&&(_=p[o])){const m=_.length;if(m)for(let E=0;E<m;E+=2){const Oe=t[_[E]][_[E+1]].subscribe(i),Nt=f.length;f.push(i,Oe),u&&u.push(o,r.index,Nt,-(Nt+1))}}}(i,o,o[G],s,e,t,!!n,r),Z}function Dg(e,t,n,r){try{return!1!==n(r)}catch(o){return Tp(e,o),!1}}function Fc(e,t,n,r,o){return function i(s){if(s===Function)return r;const a=2&e.flags?ht(e.index,t):t;0==(32&t[2])&&pc(a);let l=Dg(t,0,r,s),c=i.__ngNextListenerFn__;for(;c;)l=Dg(t,0,c,s)&&l,c=c.__ngNextListenerFn__;return o&&!1===l&&(s.preventDefault(),s.returnValue=!1),l}}function w(e=1){return function(e){return(B.lFrame.contextLView=function(e,t){for(;e>0;)t=t[15],e--;return t}(e,B.lFrame.contextLView))[8]}(e)}function oi(e,t,n){return Oc(e,"",t,"",n),oi}function Oc(e,t,n,r,o){const i=b(),s=qr(i,t,n,r);return s!==j&&_t(J(),De(),i,e,s,i[G],o,!1),Oc}function xg(e,t,n,r,o){const i=e[n+1],s=null===t;let a=r?Lt(i):yn(i),l=!1;for(;0!==a&&(!1===l||s);){const u=e[a+1];j0(e[a],t)&&(l=!0,e[a+1]=r?tc(u):Xl(u)),a=r?Lt(u):yn(u)}l&&(e[n+1]=r?Xl(i):tc(i))}function j0(e,t){return null===e||null==t||(Array.isArray(e)?e[1]:e)===t||!(!Array.isArray(e)||"string"!=typeof t)&&Nr(e,t)>=0}const Re={textEnd:0,key:0,keyEnd:0,value:0,valueEnd:0};function Ng(e){return e.substring(Re.key,Re.keyEnd)}function Rg(e,t){const n=Re.textEnd;return n===t?-1:(t=Re.keyEnd=function(e,t,n){for(;t<n&&e.charCodeAt(t)>32;)t++;return t}(e,Re.key=t,n),no(e,t,n))}function no(e,t,n){for(;t<n&&e.charCodeAt(t)<=32;)t++;return t}function ks(e,t){return function(e,t,n,r){const o=b(),i=J(),s=_n(2);i.firstUpdatePass&&Bg(i,e,s,r),t!==j&&je(o,s,t)&&jg(i,i.data[Ze()],o,o[G],e,o[s+1]=function(e,t){return null==e||("string"==typeof t?e+=t:"object"==typeof e&&(e=W(gt(e)))),e}(t,n),r,s)}(e,t,null,!0),ks}function un(e,t){for(let n=function(e){return function(e){Re.key=0,Re.keyEnd=0,Re.value=0,Re.valueEnd=0,Re.textEnd=e.length}(e),Rg(e,no(e,0,Re.textEnd))}(t);n>=0;n=Rg(t,n))pt(e,Ng(t),!0)}function Lg(e,t){return t>=e.expandoStartIndex}function Bg(e,t,n,r){const o=e.data;if(null===o[n+1]){const i=o[Ze()],s=Lg(e,n);Ug(i,r)&&null===t&&!s&&(t=!1),t=function(e,t,n,r){const o=function(e){const t=B.lFrame.currentDirectiveIndex;return-1===t?null:e[t]}(e);let i=r?t.residualClasses:t.residualStyles;if(null===o)0===(r?t.classBindings:t.styleBindings)&&(n=ii(n=Pc(null,e,t,n,r),t.attrs,r),i=null);else{const s=t.directiveStylingLast;if(-1===s||e[s]!==o)if(n=Pc(o,e,t,n,r),null===i){let l=function(e,t,n){const r=n?t.classBindings:t.styleBindings;if(0!==yn(r))return e[Lt(r)]}(e,t,r);void 0!==l&&Array.isArray(l)&&(l=Pc(null,e,t,l[1],r),l=ii(l,t.attrs,r),function(e,t,n,r){e[Lt(n?t.classBindings:t.styleBindings)]=r}(e,t,r,l))}else i=function(e,t,n){let r;const o=t.directiveEnd;for(let i=1+t.directiveStylingLast;i<o;i++)r=ii(r,e[i].hostAttrs,n);return ii(r,t.attrs,n)}(e,t,r)}return void 0!==i&&(r?t.residualClasses=i:t.residualStyles=i),n}(o,i,t,r),function(e,t,n,r,o,i){let s=i?t.classBindings:t.styleBindings,a=Lt(s),l=yn(s);e[r]=n;let u,c=!1;if(Array.isArray(n)){const d=n;u=d[1],(null===u||Nr(d,u)>0)&&(c=!0)}else u=n;if(o)if(0!==l){const f=Lt(e[a+1]);e[r+1]=Ts(f,a),0!==f&&(e[f+1]=ec(e[f+1],r)),e[a+1]=function(e,t){return 131071&e|t<<17}(e[a+1],r)}else e[r+1]=Ts(a,0),0!==a&&(e[a+1]=ec(e[a+1],r)),a=r;else e[r+1]=Ts(l,0),0===a?a=r:e[l+1]=ec(e[l+1],r),l=r;c&&(e[r+1]=Xl(e[r+1])),xg(e,u,r,!0),xg(e,u,r,!1),function(e,t,n,r,o){const i=o?e.residualClasses:e.residualStyles;null!=i&&"string"==typeof t&&Nr(i,t)>=0&&(n[r+1]=tc(n[r+1]))}(t,u,e,r,i),s=Ts(a,l),i?t.classBindings=s:t.styleBindings=s}(o,i,t,n,s,r)}}function Pc(e,t,n,r,o){let i=null;const s=n.directiveEnd;let a=n.directiveStylingLast;for(-1===a?a=n.directiveStart:a++;a<s&&(i=t[a],r=ii(r,i.hostAttrs,o),i!==e);)a++;return null!==e&&(n.directiveStylingLast=a),r}function ii(e,t,n){const r=n?1:2;let o=-1;if(null!==t)for(let i=0;i<t.length;i++){const s=t[i];"number"==typeof s?o=s:o===r&&(Array.isArray(e)||(e=void 0===e?[]:["",e]),pt(e,s,!!n||t[++i]))}return void 0===e?null:e}function jg(e,t,n,r,o,i,s,a){if(!(3&t.type))return;const l=e.data,c=l[a+1];Ls(function(e){return 1==(1&e)}(c)?$g(l,t,n,o,yn(c),s):void 0)||(Ls(i)||function(e){return 2==(2&e)}(c)&&(i=$g(l,null,n,o,a,s)),function(e,t,n,r,o){const i=ve(e);if(t)o?i?e.addClass(n,r):n.classList.add(r):i?e.removeClass(n,r):n.classList.remove(r);else{let s=-1===r.indexOf("-")?void 0:mt.DashCase;if(null==o)i?e.removeStyle(n,r,s):n.style.removeProperty(r);else{const a="string"==typeof o&&o.endsWith("!important");a&&(o=o.slice(0,-10),s|=mt.Important),i?e.setStyle(n,r,o,s):n.style.setProperty(r,o,a?"important":"")}}}(r,s,es(Ze(),n),o,i))}function $g(e,t,n,r,o,i){const s=null===t;let a;for(;o>0;){const l=e[o],c=Array.isArray(l),u=c?l[1]:l,d=null===u;let f=n[o+1];f===j&&(f=d?ie:void 0);let h=d?Il(f,r):u===r?f:void 0;if(c&&!Ls(h)&&(h=Il(l,r)),Ls(h)&&(a=h,s))return a;const p=e[o+1];o=s?Lt(p):yn(p)}if(null!==t){let l=i?t.residualClasses:t.residualStyles;null!=l&&(a=Il(l,r))}return a}function Ls(e){return void 0!==e}function Ug(e,t){return 0!=(e.flags&(t?16:32))}function M(e,t=""){const n=b(),r=J(),o=e+20,i=r.firstCreatePass?Br(r,o,1,t,null):r.data[o],s=n[o]=function(e,t){return ve(e)?e.createText(t):e.createTextNode(t)}(n[G],t);Is(r,n,s,i),Zt(i,!1)}function P(e){return oe("",e,""),P}function oe(e,t,n){const r=b(),o=qr(r,e,t,n);return o!==j&&vn(r,Ze(),o),oe}function Ln(e,t,n){!function(e,t,n,r){const o=J(),i=_n(2);o.firstUpdatePass&&Bg(o,null,i,r);const s=b();if(n!==j&&je(s,i,n)){const a=o.data[Ze()];if(Ug(a,r)&&!Lg(o,i)){let l=r?a.classesWithoutHost:a.stylesWithoutHost;null!==l&&(n=Qa(l,n||"")),Nc(o,a,s,n,r)}else!function(e,t,n,r,o,i,s,a){o===j&&(o=ie);let l=0,c=0,u=0<o.length?o[0]:null,d=0<i.length?i[0]:null;for(;null!==u||null!==d;){const f=l<o.length?o[l+1]:void 0,h=c<i.length?i[c+1]:void 0;let _,p=null;u===d?(l+=2,c+=2,f!==h&&(p=d,_=h)):null===d||null!==u&&u<d?(l+=2,p=u):(c+=2,p=d,_=h),null!==p&&jg(e,t,n,r,p,_,s,a),u=l<o.length?o[l]:null,d=c<i.length?i[c]:null}}(o,a,s,s[G],s[i+1],s[i+1]=function(e,t,n){if(null==n||""===n)return ie;const r=[],o=gt(n);if(Array.isArray(o))for(let i=0;i<o.length;i++)e(r,o[i],!0);else if("object"==typeof o)for(const i in o)o.hasOwnProperty(i)&&e(r,i,o[i]);else"string"==typeof o&&t(r,o);return r}(e,t,n),r,i)}}(pt,un,qr(b(),e,t,n),!0)}const lr=void 0;var DI=["en",[["a","p"],["AM","PM"],lr],[["AM","PM"],lr,lr],[["S","M","T","W","T","F","S"],["Sun","Mon","Tue","Wed","Thu","Fri","Sat"],["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"],["Su","Mo","Tu","We","Th","Fr","Sa"]],lr,[["J","F","M","A","M","J","J","A","S","O","N","D"],["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],["January","February","March","April","May","June","July","August","September","October","November","December"]],lr,[["B","A"],["BC","AD"],["Before Christ","Anno Domini"]],0,[6,0],["M/d/yy","MMM d, y","MMMM d, y","EEEE, MMMM d, y"],["h:mm a","h:mm:ss a","h:mm:ss a z","h:mm:ss a zzzz"],["{1}, {0}",lr,"{1} 'at' {0}",lr],[".",",",";","%","+","-","E","\xd7","\u2030","\u221e","NaN",":"],["#,##0.###","#,##0%","\xa4#,##0.00","#E0"],"USD","$","US Dollar",{},"ltr",function(e){const t=Math.floor(Math.abs(e)),n=e.toString().replace(/^[^.]*\.?/,"").length;return 1===t&&0===n?1:5}];let ro={};function um(e){return e in ro||(ro[e]=ne.ng&&ne.ng.common&&ne.ng.common.locales&&ne.ng.common.locales[e]),ro[e]}var A=(()=>((A=A||{})[A.LocaleId=0]="LocaleId",A[A.DayPeriodsFormat=1]="DayPeriodsFormat",A[A.DayPeriodsStandalone=2]="DayPeriodsStandalone",A[A.DaysFormat=3]="DaysFormat",A[A.DaysStandalone=4]="DaysStandalone",A[A.MonthsFormat=5]="MonthsFormat",A[A.MonthsStandalone=6]="MonthsStandalone",A[A.Eras=7]="Eras",A[A.FirstDayOfWeek=8]="FirstDayOfWeek",A[A.WeekendRange=9]="WeekendRange",A[A.DateFormat=10]="DateFormat",A[A.TimeFormat=11]="TimeFormat",A[A.DateTimeFormat=12]="DateTimeFormat",A[A.NumberSymbols=13]="NumberSymbols",A[A.NumberFormats=14]="NumberFormats",A[A.CurrencyCode=15]="CurrencyCode",A[A.CurrencySymbol=16]="CurrencySymbol",A[A.CurrencyName=17]="CurrencyName",A[A.Currencies=18]="Currencies",A[A.Directionality=19]="Directionality",A[A.PluralCase=20]="PluralCase",A[A.ExtraData=21]="ExtraData",A))();const Bs="en-US";let dm=Bs;function Vc(e){ut(e,"Expected localeId to be defined"),"string"==typeof e&&(dm=e.toLowerCase().replace(/_/g,"-"))}function Bc(e,t,n,r,o){if(e=N(e),Array.isArray(e))for(let i=0;i<e.length;i++)Bc(e[i],t,n,r,o);else{const i=J(),s=b();let a=Gr(e)?e:N(e.provide),l=Rp(e);const c=xe(),u=1048575&c.providerIndexes,d=c.directiveStart,f=c.providerIndexes>>20;if(Gr(e)||!e.multi){const h=new Fo(l,o,I),p=jc(a,t,o?u:u+f,d);-1===p?(fs(Po(c,s),i,a),Hc(i,e,t.length),t.push(a),c.directiveStart++,c.directiveEnd++,o&&(c.providerIndexes+=1048576),n.push(h),s.push(h)):(n[p]=h,s[p]=h)}else{const h=jc(a,t,u+f,d),p=jc(a,t,u,u+f),_=h>=0&&n[h],m=p>=0&&n[p];if(o&&!m||!o&&!_){fs(Po(c,s),i,a);const E=function(e,t,n,r,o){const i=new Fo(e,n,I);return i.multi=[],i.index=t,i.componentProviders=0,Pm(i,o,r&&!n),i}(o?y1:_1,n.length,o,r,l);!o&&m&&(n[p].providerFactory=E),Hc(i,e,t.length,0),t.push(a),c.directiveStart++,c.directiveEnd++,o&&(c.providerIndexes+=1048576),n.push(E),s.push(E)}else Hc(i,e,h>-1?h:p,Pm(n[o?p:h],l,!o&&r));!o&&r&&m&&n[p].componentProviders++}}}function Hc(e,t,n,r){const o=Gr(t);if(o||function(e){return!!e.useClass}(t)){const s=(t.useClass||t).prototype.ngOnDestroy;if(s){const a=e.destroyHooks||(e.destroyHooks=[]);if(!o&&t.multi){const l=a.indexOf(n);-1===l?a.push(n,[r,s]):a[l+1].push(r,s)}else a.push(n,s)}}}function Pm(e,t,n){return n&&e.componentProviders++,e.multi.push(t)-1}function jc(e,t,n,r){for(let o=n;o<r;o++)if(t[o]===e)return o;return-1}function _1(e,t,n,r){return $c(this.multi,[])}function y1(e,t,n,r){const o=this.multi;let i;if(this.providerFactory){const s=this.providerFactory.componentProviders,a=Vo(n,n[1],this.providerFactory.index,r);i=a.slice(0,s),$c(o,i);for(let l=s;l<a.length;l++)i.push(a[l])}else i=[],$c(o,i);return i}function $c(e,t){for(let n=0;n<e.length;n++)t.push((0,e[n])());return t}function _e(e,t=[]){return n=>{n.providersResolver=(r,o)=>function(e,t,n){const r=J();if(r.firstCreatePass){const o=Vt(e);Bc(n,r.data,r.blueprint,o,!0),Bc(t,r.data,r.blueprint,o,!1)}}(r,o?o(e):e,t)}}class Vm{}const Lm="ngComponent";class D1{resolveComponentFactory(t){throw function(e){const t=Error(`No component factory found for ${W(e)}. Did you add it to @NgModule.entryComponents?`);return t[Lm]=e,t}(t)}}let io=(()=>{class e{}return e.NULL=new D1,e})();function Gs(...e){}function so(e,t){return new $e(bt(e,t))}const w1=function(){return so(xe(),b())};let $e=(()=>{class e{constructor(n){this.nativeElement=n}}return e.__NG_ELEMENT_ID__=w1,e})();class zs{}let cr=(()=>{class e{}return e.__NG_ELEMENT_ID__=()=>M1(),e})();const M1=function(){const e=b(),n=ht(xe().index,e);return function(e){return e[G]}(Yt(n)?n:e)};let Gc=(()=>{class e{}return e.\u0275prov=te({token:e,providedIn:"root",factory:()=>null}),e})();class Ws{constructor(t){this.full=t,this.major=t.split(".")[0],this.minor=t.split(".")[1],this.patch=t.split(".").slice(2).join(".")}}const Hm=new Ws("12.2.6");class jm{constructor(){}supports(t){return ni(t)}create(t){return new x1(t)}}const S1=(e,t)=>t;class x1{constructor(t){this.length=0,this._linkedRecords=null,this._unlinkedRecords=null,this._previousItHead=null,this._itHead=null,this._itTail=null,this._additionsHead=null,this._additionsTail=null,this._movesHead=null,this._movesTail=null,this._removalsHead=null,this._removalsTail=null,this._identityChangesHead=null,this._identityChangesTail=null,this._trackByFn=t||S1}forEachItem(t){let n;for(n=this._itHead;null!==n;n=n._next)t(n)}forEachOperation(t){let n=this._itHead,r=this._removalsHead,o=0,i=null;for(;n||r;){const s=!r||n&&n.currentIndex<Um(r,o,i)?n:r,a=Um(s,o,i),l=s.currentIndex;if(s===r)o--,r=r._nextRemoved;else if(n=n._next,null==s.previousIndex)o++;else{i||(i=[]);const c=a-o,u=l-o;if(c!=u){for(let f=0;f<c;f++){const h=f<i.length?i[f]:i[f]=0,p=h+f;u<=p&&p<c&&(i[f]=h+1)}i[s.previousIndex]=u-c}}a!==l&&t(s,a,l)}}forEachPreviousItem(t){let n;for(n=this._previousItHead;null!==n;n=n._nextPrevious)t(n)}forEachAddedItem(t){let n;for(n=this._additionsHead;null!==n;n=n._nextAdded)t(n)}forEachMovedItem(t){let n;for(n=this._movesHead;null!==n;n=n._nextMoved)t(n)}forEachRemovedItem(t){let n;for(n=this._removalsHead;null!==n;n=n._nextRemoved)t(n)}forEachIdentityChange(t){let n;for(n=this._identityChangesHead;null!==n;n=n._nextIdentityChange)t(n)}diff(t){if(null==t&&(t=[]),!ni(t))throw new Error(`Error trying to diff '${W(t)}'. Only arrays and iterables are allowed`);return this.check(t)?this:null}onDestroy(){}check(t){this._reset();let o,i,s,n=this._itHead,r=!1;if(Array.isArray(t)){this.length=t.length;for(let a=0;a<this.length;a++)i=t[a],s=this._trackByFn(a,i),null!==n&&Object.is(n.trackById,s)?(r&&(n=this._verifyReinsertion(n,i,s,a)),Object.is(n.item,i)||this._addIdentityChange(n,i)):(n=this._mismatch(n,i,s,a),r=!0),n=n._next}else o=0,function(e,t){if(Array.isArray(e))for(let n=0;n<e.length;n++)t(e[n]);else{const n=e[zr()]();let r;for(;!(r=n.next()).done;)t(r.value)}}(t,a=>{s=this._trackByFn(o,a),null!==n&&Object.is(n.trackById,s)?(r&&(n=this._verifyReinsertion(n,a,s,o)),Object.is(n.item,a)||this._addIdentityChange(n,a)):(n=this._mismatch(n,a,s,o),r=!0),n=n._next,o++}),this.length=o;return this._truncate(n),this.collection=t,this.isDirty}get isDirty(){return null!==this._additionsHead||null!==this._movesHead||null!==this._removalsHead||null!==this._identityChangesHead}_reset(){if(this.isDirty){let t;for(t=this._previousItHead=this._itHead;null!==t;t=t._next)t._nextPrevious=t._next;for(t=this._additionsHead;null!==t;t=t._nextAdded)t.previousIndex=t.currentIndex;for(this._additionsHead=this._additionsTail=null,t=this._movesHead;null!==t;t=t._nextMoved)t.previousIndex=t.currentIndex;this._movesHead=this._movesTail=null,this._removalsHead=this._removalsTail=null,this._identityChangesHead=this._identityChangesTail=null}}_mismatch(t,n,r,o){let i;return null===t?i=this._itTail:(i=t._prev,this._remove(t)),null!==(t=null===this._unlinkedRecords?null:this._unlinkedRecords.get(r,null))?(Object.is(t.item,n)||this._addIdentityChange(t,n),this._reinsertAfter(t,i,o)):null!==(t=null===this._linkedRecords?null:this._linkedRecords.get(r,o))?(Object.is(t.item,n)||this._addIdentityChange(t,n),this._moveAfter(t,i,o)):t=this._addAfter(new N1(n,r),i,o),t}_verifyReinsertion(t,n,r,o){let i=null===this._unlinkedRecords?null:this._unlinkedRecords.get(r,null);return null!==i?t=this._reinsertAfter(i,t._prev,o):t.currentIndex!=o&&(t.currentIndex=o,this._addToMoves(t,o)),t}_truncate(t){for(;null!==t;){const n=t._next;this._addToRemovals(this._unlink(t)),t=n}null!==this._unlinkedRecords&&this._unlinkedRecords.clear(),null!==this._additionsTail&&(this._additionsTail._nextAdded=null),null!==this._movesTail&&(this._movesTail._nextMoved=null),null!==this._itTail&&(this._itTail._next=null),null!==this._removalsTail&&(this._removalsTail._nextRemoved=null),null!==this._identityChangesTail&&(this._identityChangesTail._nextIdentityChange=null)}_reinsertAfter(t,n,r){null!==this._unlinkedRecords&&this._unlinkedRecords.remove(t);const o=t._prevRemoved,i=t._nextRemoved;return null===o?this._removalsHead=i:o._nextRemoved=i,null===i?this._removalsTail=o:i._prevRemoved=o,this._insertAfter(t,n,r),this._addToMoves(t,r),t}_moveAfter(t,n,r){return this._unlink(t),this._insertAfter(t,n,r),this._addToMoves(t,r),t}_addAfter(t,n,r){return this._insertAfter(t,n,r),this._additionsTail=null===this._additionsTail?this._additionsHead=t:this._additionsTail._nextAdded=t,t}_insertAfter(t,n,r){const o=null===n?this._itHead:n._next;return t._next=o,t._prev=n,null===o?this._itTail=t:o._prev=t,null===n?this._itHead=t:n._next=t,null===this._linkedRecords&&(this._linkedRecords=new $m),this._linkedRecords.put(t),t.currentIndex=r,t}_remove(t){return this._addToRemovals(this._unlink(t))}_unlink(t){null!==this._linkedRecords&&this._linkedRecords.remove(t);const n=t._prev,r=t._next;return null===n?this._itHead=r:n._next=r,null===r?this._itTail=n:r._prev=n,t}_addToMoves(t,n){return t.previousIndex===n||(this._movesTail=null===this._movesTail?this._movesHead=t:this._movesTail._nextMoved=t),t}_addToRemovals(t){return null===this._unlinkedRecords&&(this._unlinkedRecords=new $m),this._unlinkedRecords.put(t),t.currentIndex=null,t._nextRemoved=null,null===this._removalsTail?(this._removalsTail=this._removalsHead=t,t._prevRemoved=null):(t._prevRemoved=this._removalsTail,this._removalsTail=this._removalsTail._nextRemoved=t),t}_addIdentityChange(t,n){return t.item=n,this._identityChangesTail=null===this._identityChangesTail?this._identityChangesHead=t:this._identityChangesTail._nextIdentityChange=t,t}}class N1{constructor(t,n){this.item=t,this.trackById=n,this.currentIndex=null,this.previousIndex=null,this._nextPrevious=null,this._prev=null,this._next=null,this._prevDup=null,this._nextDup=null,this._prevRemoved=null,this._nextRemoved=null,this._nextAdded=null,this._nextMoved=null,this._nextIdentityChange=null}}class R1{constructor(){this._head=null,this._tail=null}add(t){null===this._head?(this._head=this._tail=t,t._nextDup=null,t._prevDup=null):(this._tail._nextDup=t,t._prevDup=this._tail,t._nextDup=null,this._tail=t)}get(t,n){let r;for(r=this._head;null!==r;r=r._nextDup)if((null===n||n<=r.currentIndex)&&Object.is(r.trackById,t))return r;return null}remove(t){const n=t._prevDup,r=t._nextDup;return null===n?this._head=r:n._nextDup=r,null===r?this._tail=n:r._prevDup=n,null===this._head}}class $m{constructor(){this.map=new Map}put(t){const n=t.trackById;let r=this.map.get(n);r||(r=new R1,this.map.set(n,r)),r.add(t)}get(t,n){const o=this.map.get(t);return o?o.get(t,n):null}remove(t){const n=t.trackById;return this.map.get(n).remove(t)&&this.map.delete(n),t}get isEmpty(){return 0===this.map.size}clear(){this.map.clear()}}function Um(e,t,n){const r=e.previousIndex;if(null===r)return r;let o=0;return n&&r<n.length&&(o=n[r]),r+t+o}class Gm{constructor(){}supports(t){return t instanceof Map||Mc(t)}create(){return new F1}}class F1{constructor(){this._records=new Map,this._mapHead=null,this._appendAfter=null,this._previousMapHead=null,this._changesHead=null,this._changesTail=null,this._additionsHead=null,this._additionsTail=null,this._removalsHead=null,this._removalsTail=null}get isDirty(){return null!==this._additionsHead||null!==this._changesHead||null!==this._removalsHead}forEachItem(t){let n;for(n=this._mapHead;null!==n;n=n._next)t(n)}forEachPreviousItem(t){let n;for(n=this._previousMapHead;null!==n;n=n._nextPrevious)t(n)}forEachChangedItem(t){let n;for(n=this._changesHead;null!==n;n=n._nextChanged)t(n)}forEachAddedItem(t){let n;for(n=this._additionsHead;null!==n;n=n._nextAdded)t(n)}forEachRemovedItem(t){let n;for(n=this._removalsHead;null!==n;n=n._nextRemoved)t(n)}diff(t){if(t){if(!(t instanceof Map||Mc(t)))throw new Error(`Error trying to diff '${W(t)}'. Only maps and objects are allowed`)}else t=new Map;return this.check(t)?this:null}onDestroy(){}check(t){this._reset();let n=this._mapHead;if(this._appendAfter=null,this._forEach(t,(r,o)=>{if(n&&n.key===o)this._maybeAddToChanges(n,r),this._appendAfter=n,n=n._next;else{const i=this._getOrCreateRecordForKey(o,r);n=this._insertBeforeOrAppend(n,i)}}),n){n._prev&&(n._prev._next=null),this._removalsHead=n;for(let r=n;null!==r;r=r._nextRemoved)r===this._mapHead&&(this._mapHead=null),this._records.delete(r.key),r._nextRemoved=r._next,r.previousValue=r.currentValue,r.currentValue=null,r._prev=null,r._next=null}return this._changesTail&&(this._changesTail._nextChanged=null),this._additionsTail&&(this._additionsTail._nextAdded=null),this.isDirty}_insertBeforeOrAppend(t,n){if(t){const r=t._prev;return n._next=t,n._prev=r,t._prev=n,r&&(r._next=n),t===this._mapHead&&(this._mapHead=n),this._appendAfter=t,t}return this._appendAfter?(this._appendAfter._next=n,n._prev=this._appendAfter):this._mapHead=n,this._appendAfter=n,null}_getOrCreateRecordForKey(t,n){if(this._records.has(t)){const o=this._records.get(t);this._maybeAddToChanges(o,n);const i=o._prev,s=o._next;return i&&(i._next=s),s&&(s._prev=i),o._next=null,o._prev=null,o}const r=new O1(t);return this._records.set(t,r),r.currentValue=n,this._addToAdditions(r),r}_reset(){if(this.isDirty){let t;for(this._previousMapHead=this._mapHead,t=this._previousMapHead;null!==t;t=t._next)t._nextPrevious=t._next;for(t=this._changesHead;null!==t;t=t._nextChanged)t.previousValue=t.currentValue;for(t=this._additionsHead;null!=t;t=t._nextAdded)t.previousValue=t.currentValue;this._changesHead=this._changesTail=null,this._additionsHead=this._additionsTail=null,this._removalsHead=null}}_maybeAddToChanges(t,n){Object.is(n,t.currentValue)||(t.previousValue=t.currentValue,t.currentValue=n,this._addToChanges(t))}_addToAdditions(t){null===this._additionsHead?this._additionsHead=this._additionsTail=t:(this._additionsTail._nextAdded=t,this._additionsTail=t)}_addToChanges(t){null===this._changesHead?this._changesHead=this._changesTail=t:(this._changesTail._nextChanged=t,this._changesTail=t)}_forEach(t,n){t instanceof Map?t.forEach(n):Object.keys(t).forEach(r=>n(t[r],r))}}class O1{constructor(t){this.key=t,this.previousValue=null,this.currentValue=null,this._nextPrevious=null,this._next=null,this._prev=null,this._nextAdded=null,this._nextRemoved=null,this._nextChanged=null}}function zm(){return new ui([new jm])}let ui=(()=>{class e{constructor(n){this.factories=n}static create(n,r){if(null!=r){const o=r.factories.slice();n=n.concat(o)}return new e(n)}static extend(n){return{provide:e,useFactory:r=>e.create(n,r||zm()),deps:[[e,new rr,new en]]}}find(n){const r=this.factories.find(o=>o.supports(n));if(null!=r)return r;throw new Error(`Cannot find a differ supporting object '${n}' of type '${function(e){return e.name||typeof e}(n)}'`)}}return e.\u0275prov=te({token:e,providedIn:"root",factory:zm}),e})();function Wm(){return new ao([new Gm])}let ao=(()=>{class e{constructor(n){this.factories=n}static create(n,r){if(r){const o=r.factories.slice();n=n.concat(o)}return new e(n)}static extend(n){return{provide:e,useFactory:r=>e.create(n,r||Wm()),deps:[[e,new rr,new en]]}}find(n){const r=this.factories.find(o=>o.supports(n));if(r)return r;throw new Error(`Cannot find a differ supporting object '${n}'`)}}return e.\u0275prov=te({token:e,providedIn:"root",factory:Wm}),e})();function qs(e,t,n,r,o=!1){for(;null!==n;){const i=t[n.index];if(null!==i&&r.push(Me(i)),Pt(i))for(let a=10;a<i.length;a++){const l=i[a],c=l[1].firstChild;null!==c&&qs(l[1],l,c,r)}const s=n.type;if(8&s)qs(e,t,n.child,r);else if(32&s){const a=$l(n,t);let l;for(;l=a();)r.push(l)}else if(16&s){const a=Uh(t,n);if(Array.isArray(a))r.push(...a);else{const l=Ko(t[16]);qs(l[1],l,a,r,!0)}}n=o?n.projectionNext:n.next}return r}class di{constructor(t,n){this._lView=t,this._cdRefInjectingView=n,this._appRef=null,this._attachedToViewContainer=!1}get rootNodes(){const t=this._lView,n=t[1];return qs(n,t,n.firstChild,[])}get context(){return this._lView[8]}set context(t){this._lView[8]=t}get destroyed(){return 256==(256&this._lView[2])}destroy(){if(this._appRef)this._appRef.detachView(this);else if(this._attachedToViewContainer){const t=this._lView[3];if(Pt(t)){const n=t[8],r=n?n.indexOf(this):-1;r>-1&&(ql(t,r),tr(n,r))}this._attachedToViewContainer=!1}Oh(this._lView[1],this._lView)}onDestroy(t){!function(e,t,n,r){const o=wp(t);null===n?o.push(r):(o.push(n),e.firstCreatePass&&Ip(e).push(r,o.length-1))}(this._lView[1],this._lView,null,t)}markForCheck(){pc(this._cdRefInjectingView||this._lView)}detach(){this._lView[2]&=-129}reattach(){this._lView[2]|=128}detectChanges(){mc(this._lView[1],this._lView,this.context)}checkNoChanges(){!function(e,t,n){ns(!0);try{mc(e,t,n)}finally{ns(!1)}}(this._lView[1],this._lView,this.context)}attachToViewContainerRef(){if(this._appRef)throw new Error("This view is already attached directly to the ApplicationRef!");this._attachedToViewContainer=!0}detachFromAppRef(){this._appRef=null,function(e,t){Yo(e,t,t[G],2,null,null)}(this._lView[1],this._lView)}attachToAppRef(t){if(this._attachedToViewContainer)throw new Error("This view is already attached to a ViewContainer!");this._appRef=t}}class V1 extends di{constructor(t){super(t),this._view=t}detectChanges(){Ep(this._view)}checkNoChanges(){!function(e){ns(!0);try{Ep(e)}finally{ns(!1)}}(this._view)}get context(){return null}}const $1=[new Gm],G1=new ui([new jm]),z1=new ao($1),q1=function(){return function(e,t){return 4&e.type?new K1(t,e,so(e,t)):null}(xe(),b())};let bn=(()=>{class e{}return e.__NG_ELEMENT_ID__=q1,e})();const Q1=bn,K1=class extends Q1{constructor(t,n,r){super(),this._declarationLView=t,this._declarationTContainer=n,this.elementRef=r}createEmbeddedView(t){const n=this._declarationTContainer.tViews,r=Zo(this._declarationLView,n,t,16,null,n.declTNode,null,null,null,null);r[17]=this._declarationLView[this._declarationTContainer.index];const i=this._declarationLView[19];return null!==i&&(r[19]=i.createEmbeddedView(n)),Jo(n,r,t),new di(r)}};class ur{}const X1=function(){return function(e,t){let n;const r=t[e.index];if(Pt(r))n=r;else{let o;if(8&e.type)o=Me(r);else{const i=t[G];o=i.createComment("");const s=bt(e,t);sr(i,ws(i,s),o,function(e,t){return ve(e)?e.nextSibling(t):t.nextSibling}(i,s),!1)}t[e.index]=n=bp(r,t,o,e),Ns(t,n)}return new qm(n,e,t)}(xe(),b())};let dn=(()=>{class e{}return e.__NG_ELEMENT_ID__=X1,e})();const tM=dn,qm=class extends tM{constructor(t,n,r){super(),this._lContainer=t,this._hostTNode=n,this._hostLView=r}get element(){return so(this._hostTNode,this._hostLView)}get injector(){return new Tr(this._hostTNode,this._hostLView)}get parentInjector(){const t=ds(this._hostTNode,this._hostLView);if(Lf(t)){const n=Mr(t,this._hostLView),r=Ir(t);return new Tr(n[1].data[r+8],n)}return new Tr(null,this._hostLView)}clear(){for(;this.length>0;)this.remove(this.length-1)}get(t){const n=Qm(this._lContainer);return null!==n&&n[t]||null}get length(){return this._lContainer.length-10}createEmbeddedView(t,n,r){const o=t.createEmbeddedView(n||{});return this.insert(o,r),o}createComponent(t,n,r,o,i){const s=r||this.parentInjector;if(!i&&null==t.ngModule&&s){const l=s.get(ur,null);l&&(i=l)}const a=t.create(s,o,void 0,i);return this.insert(a.hostView,n),a}insert(t,n){const r=t._lView,o=r[1];if(function(e){return Pt(e[3])}(r)){const u=this.indexOf(t);if(-1!==u)this.detach(u);else{const d=r[3],f=new qm(d,d[6],d[3]);f.detach(f.indexOf(t))}}const i=this._adjustIndex(n),s=this._lContainer;!function(e,t,n,r){const o=10+r,i=n.length;r>0&&(n[o-1][4]=t),r<i-10?(t[4]=n[o],gs(n,10+r,t)):(n.push(t),t[4]=null),t[3]=n;const s=t[17];null!==s&&n!==s&&function(e,t){const n=e[9];t[16]!==t[3][3][16]&&(e[2]=!0),null===n?e[9]=[t]:n.push(t)}(s,t);const a=t[19];null!==a&&a.insertView(e),t[2]|=128}(o,r,s,i);const a=Yl(i,s),l=r[G],c=ws(l,s[7]);return null!==c&&function(e,t,n,r,o,i){r[0]=o,r[6]=t,Yo(e,r,n,1,o,i)}(o,s[6],l,r,c,a),t.attachToViewContainerRef(),gs(zc(s),i,t),t}move(t,n){return this.insert(t,n)}indexOf(t){const n=Qm(this._lContainer);return null!==n?n.indexOf(t):-1}remove(t){const n=this._adjustIndex(t,-1),r=ql(this._lContainer,n);r&&(tr(zc(this._lContainer),n),Oh(r[1],r))}detach(t){const n=this._adjustIndex(t,-1),r=ql(this._lContainer,n);return r&&null!=tr(zc(this._lContainer),n)?new di(r):null}_adjustIndex(t,n=0){return null==t?this.length+n:t}};function Qm(e){return e[8]}function zc(e){return e[8]||(e[8]=[])}const fo={};class g_ extends io{constructor(t){super(),this.ngModule=t}resolveComponentFactory(t){const n=Ke(t);return new __(n,this.ngModule)}}function m_(e){const t=[];for(let n in e)e.hasOwnProperty(n)&&t.push({propName:e[n],templateName:n});return t}const ZM=new X("SCHEDULER_TOKEN",{providedIn:"root",factory:()=>Mh});class __ extends Vm{constructor(t,n){super(),this.componentDef=t,this.ngModule=n,this.componentType=t.type,this.selector=function(e){return e.map(HE).join(",")}(t.selectors),this.ngContentSelectors=t.ngContentSelectors?t.ngContentSelectors:[],this.isBoundToModule=!!n}get inputs(){return m_(this.componentDef.inputs)}get outputs(){return m_(this.componentDef.outputs)}create(t,n,r,o){const i=(o=o||this.ngModule)?function(e,t){return{get:(n,r,o)=>{const i=e.get(n,fo,o);return i!==fo||r===fo?i:t.get(n,r,o)}}}(t,o.injector):t,s=i.get(zs,Df),a=i.get(Gc,null),l=s.createRenderer(null,this.componentDef),c=this.componentDef.selectors[0][0]||"div",u=r?function(e,t,n){if(ve(e))return e.selectRootElement(t,n===Se.ShadowDom);let r="string"==typeof t?e.querySelector(t):t;return r.textContent="",r}(l,r,this.componentDef.encapsulation):Wl(s.createRenderer(null,this.componentDef),c,function(e){const t=e.toLowerCase();return"svg"===t?Cf:"math"===t?"http://www.w3.org/1998/MathML/":null}(c)),d=this.componentDef.onPush?576:528,f=function(e,t){return{components:[],scheduler:e||Mh,clean:ww,playerHandler:t||null,flags:0}}(),h=xs(0,null,null,1,0,null,null,null,null,null),p=Zo(null,h,f,d,null,null,s,l,a,i);let _,m;rs(p);try{const E=function(e,t,n,r,o,i){const s=n[1];n[20]=e;const l=Br(s,20,2,"#host",null),c=l.mergedAttrs=t.hostAttrs;null!==c&&(Rs(l,c,!0),null!==e&&(ls(o,e,c),null!==l.classes&&Jl(o,e,l.classes),null!==l.styles&&Wh(o,e,l.styles)));const u=r.createRenderer(e,t),d=Zo(n,dp(t),null,t.onPush?64:16,n[20],l,r,u,i||null,null);return s.firstCreatePass&&(fs(Po(l,n),s,t.type),Cp(s,l),vp(l,n.length,1)),Ns(n,d),n[20]=d}(u,this.componentDef,p,s,l);if(u)if(r)ls(l,u,["ng-version",Hm.full]);else{const{attrs:v,classes:x}=function(e){const t=[],n=[];let r=1,o=2;for(;r<e.length;){let i=e[r];if("string"==typeof i)2===o?""!==i&&t.push(i,e[++r]):8===o&&n.push(i);else{if(!kt(o))break;o=i}r++}return{attrs:t,classes:n}}(this.componentDef.selectors[0]);v&&ls(l,u,v),x&&x.length>0&&Jl(l,u,x.join(" "))}if(m=al(h,20),void 0!==n){const v=m.projection=[];for(let x=0;x<this.ngContentSelectors.length;x++){const R=n[x];v.push(null!=R?Array.from(R):null)}}_=function(e,t,n,r,o){const i=n[1],s=function(e,t,n){const r=xe();e.firstCreatePass&&(n.providersResolver&&n.providersResolver(n),Dp(e,r,t,Hr(e,t,1,null),n));const o=Vo(t,e,r.directiveStart,r);He(o,t);const i=bt(r,t);return i&&He(i,t),o}(i,n,t);if(r.components.push(s),e[8]=s,o&&o.forEach(l=>l(s,t)),t.contentQueries){const l=xe();t.contentQueries(1,s,l.directiveStart)}const a=xe();return!i.firstCreatePass||null===t.hostBindings&&null===t.hostAttrs||(Fn(a.index),_p(n[1],a,0,a.directiveStart,a.directiveEnd,t),yp(t,s)),s}(E,this.componentDef,p,f,[Yw]),Jo(h,p,null)}finally{os()}return new eT(this.componentType,_,so(m,p),p,m)}}class eT extends class{}{constructor(t,n,r,o,i){super(),this.location=r,this._rootLView=o,this._tNode=i,this.instance=n,this.hostView=this.changeDetectorRef=new V1(o),this.componentType=t}get injector(){return new Tr(this._tNode,this._rootLView)}destroy(){this.hostView.destroy()}onDestroy(t){this.hostView.onDestroy(t)}}const ho=new Map;class rT extends ur{constructor(t,n){super(),this._parent=n,this._bootstrapComponents=[],this.injector=this,this.destroyCbs=[],this.componentFactoryResolver=new g_(this);const r=Ct(t),o=function(e){return e[iD]||null}(t);o&&Vc(o),this._bootstrapComponents=rn(r.bootstrap),this._r3Injector=Np(t,n,[{provide:ur,useValue:this},{provide:io,useValue:this.componentFactoryResolver}],W(t)),this._r3Injector._resolveInjectorDefTypes(),this.instance=this.get(t)}get(t,n=pe.THROW_IF_NOT_FOUND,r=O.Default){return t===pe||t===ur||t===$r?this:this._r3Injector.get(t,n,r)}destroy(){const t=this._r3Injector;!t.destroyed&&t.destroy(),this.destroyCbs.forEach(n=>n()),this.destroyCbs=null}onDestroy(t){this.destroyCbs.push(t)}}class ou extends class{}{constructor(t){super(),this.moduleType=t,null!==Ct(t)&&function(e){const t=new Set;!function n(r){const o=Ct(r,!0),i=o.id;null!==i&&(function(e,t,n){if(t&&t!==n)throw new Error(`Duplicate module registered for ${e} - ${W(t)} vs ${W(t.name)}`)}(i,ho.get(i),r),ho.set(i,r));const s=rn(o.imports);for(const a of s)t.has(a)||(t.add(a),n(a))}(e)}(t)}create(t){return new rT(this.moduleType,t)}}function iu(e,t,n,r){return function(e,t,n,r,o,i){const s=t+n;return je(e,s,o)?sn(e,s+1,i?r.call(i,o):r(o)):Ci(e,s+1)}(b(),Ye(),e,t,n,r)}function su(e,t,n,r,o){return function(e,t,n,r,o,i,s){const a=t+n;return ar(e,a,o,i)?sn(e,a+2,s?r.call(s,o,i):r(o,i)):Ci(e,a+2)}(b(),Ye(),e,t,n,r,o)}function st(e,t,n,r,o,i){return b_(b(),Ye(),e,t,n,r,o,i)}function Ci(e,t){const n=e[t];return n===j?void 0:n}function b_(e,t,n,r,o,i,s,a){const l=t+n;return function(e,t,n,r,o){const i=ar(e,t,n,r);return je(e,t+2,o)||i}(e,l,o,i,s)?sn(e,l+3,a?r.call(a,o,i,s):r(o,i,s)):Ci(e,l+3)}function M_(e,t,n,r,o){const i=e+20,s=b(),a=function(e,t){return e[t]}(s,i);return function(e,t){Ht.isWrapped(t)&&(t=Ht.unwrap(t),e[B.lFrame.bindingIndex]=j);return t}(s,function(e,t){return e[1].data[t].pure}(s,i)?b_(s,Ye(),t,a.transform,n,r,o,a):a.transform(n,r,o))}function au(e){return t=>{setTimeout(e,void 0,t)}}const ze=class extends Ga{constructor(t=!1){super(),this.__isAsync=t}emit(t){super.next(t)}subscribe(t,n,r){var o,i,s;let a=t,l=n||(()=>null),c=r;if(t&&"object"==typeof t){const d=t;a=null===(o=d.next)||void 0===o?void 0:o.bind(d),l=null===(i=d.error)||void 0===i?void 0:i.bind(d),c=null===(s=d.complete)||void 0===s?void 0:s.bind(d)}this.__isAsync&&(l=au(l),a&&(a=au(a)),c&&(c=au(c)));const u=super.subscribe({next:a,error:l,complete:c});return t instanceof Ee&&t.add(u),u}};Symbol;const na=new X("Application Initializer");let go=(()=>{class e{constructor(n){this.appInits=n,this.resolve=Gs,this.reject=Gs,this.initialized=!1,this.done=!1,this.donePromise=new Promise((r,o)=>{this.resolve=r,this.reject=o})}runInitializers(){if(this.initialized)return;const n=[],r=()=>{this.done=!0,this.resolve()};if(this.appInits)for(let o=0;o<this.appInits.length;o++){const i=this.appInits[o]();if(Vs(i))n.push(i);else if(Rc(i)){const s=new Promise((a,l)=>{i.subscribe({complete:a,error:l})});n.push(s)}}Promise.all(n).then(()=>{r()}).catch(o=>{this.reject(o)}),0===n.length&&r(),this.initialized=!0}}return e.\u0275fac=function(n){return new(n||e)(Y(na,8))},e.\u0275prov=te({token:e,factory:e.\u0275fac}),e})();const Ei=new X("AppId"),rA={provide:Ei,useFactory:function(){return`${yu()}${yu()}${yu()}`},deps:[]};function yu(){return String.fromCharCode(97+Math.floor(25*Math.random()))}const K_=new X("Platform Initializer"),Cu=new X("Platform ID"),oA=new X("appBootstrapListener");let vu=(()=>{class e{log(n){console.log(n)}warn(n){console.warn(n)}}return e.\u0275fac=function(n){return new(n||e)},e.\u0275prov=te({token:e,factory:e.\u0275fac}),e})();const Hn=new X("LocaleId"),Y_=new X("DefaultCurrencyCode");class sA{constructor(t,n){this.ngModuleFactory=t,this.componentFactories=n}}const Du=function(e){return new ou(e)},aA=Du,lA=function(e){return Promise.resolve(Du(e))},Z_=function(e){const t=Du(e),r=rn(Ct(e).declarations).reduce((o,i)=>{const s=Ke(i);return s&&o.push(new __(s)),o},[]);return new sA(t,r)},cA=Z_,uA=function(e){return Promise.resolve(Z_(e))};let oa=(()=>{class e{constructor(){this.compileModuleSync=aA,this.compileModuleAsync=lA,this.compileModuleAndAllComponentsSync=cA,this.compileModuleAndAllComponentsAsync=uA}clearCache(){}clearCacheFor(n){}getModuleId(n){}}return e.\u0275fac=function(n){return new(n||e)},e.\u0275prov=te({token:e,factory:e.\u0275fac}),e})();const hA=(()=>Promise.resolve(0))();function bu(e){"undefined"==typeof Zone?hA.then(()=>{e&&e.apply(null,null)}):Zone.current.scheduleMicroTask("scheduleMicrotask",e)}class Le{constructor({enableLongStackTrace:t=!1,shouldCoalesceEventChangeDetection:n=!1,shouldCoalesceRunChangeDetection:r=!1}){if(this.hasPendingMacrotasks=!1,this.hasPendingMicrotasks=!1,this.isStable=!0,this.onUnstable=new ze(!1),this.onMicrotaskEmpty=new ze(!1),this.onStable=new ze(!1),this.onError=new ze(!1),"undefined"==typeof Zone)throw new Error("In this configuration Angular requires Zone.js");Zone.assertZonePatched();const o=this;o._nesting=0,o._outer=o._inner=Zone.current,Zone.TaskTrackingZoneSpec&&(o._inner=o._inner.fork(new Zone.TaskTrackingZoneSpec)),t&&Zone.longStackTraceZoneSpec&&(o._inner=o._inner.fork(Zone.longStackTraceZoneSpec)),o.shouldCoalesceEventChangeDetection=!r&&n,o.shouldCoalesceRunChangeDetection=r,o.lastRequestAnimationFrameId=-1,o.nativeRequestAnimationFrame=function(){let e=ne.requestAnimationFrame,t=ne.cancelAnimationFrame;if("undefined"!=typeof Zone&&e&&t){const n=e[Zone.__symbol__("OriginalDelegate")];n&&(e=n);const r=t[Zone.__symbol__("OriginalDelegate")];r&&(t=r)}return{nativeRequestAnimationFrame:e,nativeCancelAnimationFrame:t}}().nativeRequestAnimationFrame,function(e){const t=()=>{!function(e){e.isCheckStableRunning||-1!==e.lastRequestAnimationFrameId||(e.lastRequestAnimationFrameId=e.nativeRequestAnimationFrame.call(ne,()=>{e.fakeTopEventTask||(e.fakeTopEventTask=Zone.root.scheduleEventTask("fakeTopEventTask",()=>{e.lastRequestAnimationFrameId=-1,wu(e),e.isCheckStableRunning=!0,Eu(e),e.isCheckStableRunning=!1},void 0,()=>{},()=>{})),e.fakeTopEventTask.invoke()}),wu(e))}(e)};e._inner=e._inner.fork({name:"angular",properties:{isAngularZone:!0},onInvokeTask:(n,r,o,i,s,a)=>{try{return J_(e),n.invokeTask(o,i,s,a)}finally{(e.shouldCoalesceEventChangeDetection&&"eventTask"===i.type||e.shouldCoalesceRunChangeDetection)&&t(),X_(e)}},onInvoke:(n,r,o,i,s,a,l)=>{try{return J_(e),n.invoke(o,i,s,a,l)}finally{e.shouldCoalesceRunChangeDetection&&t(),X_(e)}},onHasTask:(n,r,o,i)=>{n.hasTask(o,i),r===o&&("microTask"==i.change?(e._hasPendingMicrotasks=i.microTask,wu(e),Eu(e)):"macroTask"==i.change&&(e.hasPendingMacrotasks=i.macroTask))},onHandleError:(n,r,o,i)=>(n.handleError(o,i),e.runOutsideAngular(()=>e.onError.emit(i)),!1)})}(o)}static isInAngularZone(){return!0===Zone.current.get("isAngularZone")}static assertInAngularZone(){if(!Le.isInAngularZone())throw new Error("Expected to be in Angular Zone, but it is not!")}static assertNotInAngularZone(){if(Le.isInAngularZone())throw new Error("Expected to not be in Angular Zone, but it is!")}run(t,n,r){return this._inner.run(t,n,r)}runTask(t,n,r,o){const i=this._inner,s=i.scheduleEventTask("NgZoneEvent: "+o,t,gA,Gs,Gs);try{return i.runTask(s,n,r)}finally{i.cancelTask(s)}}runGuarded(t,n,r){return this._inner.runGuarded(t,n,r)}runOutsideAngular(t){return this._outer.run(t)}}const gA={};function Eu(e){if(0==e._nesting&&!e.hasPendingMicrotasks&&!e.isStable)try{e._nesting++,e.onMicrotaskEmpty.emit(null)}finally{if(e._nesting--,!e.hasPendingMicrotasks)try{e.runOutsideAngular(()=>e.onStable.emit(null))}finally{e.isStable=!0}}}function wu(e){e.hasPendingMicrotasks=!!(e._hasPendingMicrotasks||(e.shouldCoalesceEventChangeDetection||e.shouldCoalesceRunChangeDetection)&&-1!==e.lastRequestAnimationFrameId)}function J_(e){e._nesting++,e.isStable&&(e.isStable=!1,e.onUnstable.emit(null))}function X_(e){e._nesting--,Eu(e)}class yA{constructor(){this.hasPendingMicrotasks=!1,this.hasPendingMacrotasks=!1,this.isStable=!0,this.onUnstable=new ze,this.onMicrotaskEmpty=new ze,this.onStable=new ze,this.onError=new ze}run(t,n,r){return t.apply(n,r)}runGuarded(t,n,r){return t.apply(n,r)}runOutsideAngular(t){return t()}runTask(t,n,r,o){return t.apply(n,r)}}let Iu=(()=>{class e{constructor(n){this._ngZone=n,this._pendingCount=0,this._isZoneStable=!0,this._didWork=!1,this._callbacks=[],this.taskTrackingZone=null,this._watchAngularEvents(),n.run(()=>{this.taskTrackingZone="undefined"==typeof Zone?null:Zone.current.get("TaskTrackingZone")})}_watchAngularEvents(){this._ngZone.onUnstable.subscribe({next:()=>{this._didWork=!0,this._isZoneStable=!1}}),this._ngZone.runOutsideAngular(()=>{this._ngZone.onStable.subscribe({next:()=>{Le.assertNotInAngularZone(),bu(()=>{this._isZoneStable=!0,this._runCallbacksIfReady()})}})})}increasePendingRequestCount(){return this._pendingCount+=1,this._didWork=!0,this._pendingCount}decreasePendingRequestCount(){if(this._pendingCount-=1,this._pendingCount<0)throw new Error("pending async requests below zero");return this._runCallbacksIfReady(),this._pendingCount}isStable(){return this._isZoneStable&&0===this._pendingCount&&!this._ngZone.hasPendingMacrotasks}_runCallbacksIfReady(){if(this.isStable())bu(()=>{for(;0!==this._callbacks.length;){let n=this._callbacks.pop();clearTimeout(n.timeoutId),n.doneCb(this._didWork)}this._didWork=!1});else{let n=this.getPendingTasks();this._callbacks=this._callbacks.filter(r=>!r.updateCb||!r.updateCb(n)||(clearTimeout(r.timeoutId),!1)),this._didWork=!0}}getPendingTasks(){return this.taskTrackingZone?this.taskTrackingZone.macroTasks.map(n=>({source:n.source,creationLocation:n.creationLocation,data:n.data})):[]}addCallback(n,r,o){let i=-1;r&&r>0&&(i=setTimeout(()=>{this._callbacks=this._callbacks.filter(s=>s.timeoutId!==i),n(this._didWork,this.getPendingTasks())},r)),this._callbacks.push({doneCb:n,timeoutId:i,updateCb:o})}whenStable(n,r,o){if(o&&!this.taskTrackingZone)throw new Error('Task tracking zone is required when passing an update callback to whenStable(). Is "zone.js/plugins/task-tracking" loaded?');this.addCallback(n,r,o),this._runCallbacksIfReady()}getPendingRequestCount(){return this._pendingCount}findProviders(n,r,o){return[]}}return e.\u0275fac=function(n){return new(n||e)(Y(Le))},e.\u0275prov=te({token:e,factory:e.\u0275fac}),e})(),ey=(()=>{class e{constructor(){this._applications=new Map,Mu.addToWindow(this)}registerApplication(n,r){this._applications.set(n,r)}unregisterApplication(n){this._applications.delete(n)}unregisterAllApplications(){this._applications.clear()}getTestability(n){return this._applications.get(n)||null}getAllTestabilities(){return Array.from(this._applications.values())}getAllRootElements(){return Array.from(this._applications.keys())}findTestabilityInTree(n,r=!0){return Mu.findTestabilityInTree(this,n,r)}}return e.\u0275fac=function(n){return new(n||e)},e.\u0275prov=te({token:e,factory:e.\u0275fac}),e})();class CA{addToWindow(t){}findTestabilityInTree(t,n,r){return null}}let Mu=new CA,ny=!1;let zt;const oy=new X("AllowMultipleToken");function iy(e,t,n=[]){const r=`Platform: ${t}`,o=new X(r);return(i=[])=>{let s=sy();if(!s||s.injector.get(oy,!1))if(e)e(n.concat(i).concat({provide:o,useValue:!0}));else{const a=n.concat(i).concat({provide:o,useValue:!0},{provide:Xo,useValue:"platform"});!function(e){if(zt&&!zt.destroyed&&!zt.injector.get(oy,!1))throw new Error("There can be only one platform. Destroy the previous one to create a new one.");zt=e.get(ay);const t=e.get(K_,null);t&&t.forEach(n=>n())}(pe.create({providers:a,name:r}))}return function(e){const t=sy();if(!t)throw new Error("No platform exists!");if(!t.injector.get(e,null))throw new Error("A platform with a different configuration has been created. Please destroy it first.");return t}(o)}}function sy(){return zt&&!zt.destroyed?zt:null}let ay=(()=>{class e{constructor(n){this._injector=n,this._modules=[],this._destroyListeners=[],this._destroyed=!1}bootstrapModuleFactory(n,r){const a=function(e,t){let n;return n="noop"===e?new yA:("zone.js"===e?void 0:e)||new Le({enableLongStackTrace:(ny=!0,!0),shouldCoalesceEventChangeDetection:!!(null==t?void 0:t.ngZoneEventCoalescing),shouldCoalesceRunChangeDetection:!!(null==t?void 0:t.ngZoneRunCoalescing)}),n}(r?r.ngZone:void 0,{ngZoneEventCoalescing:r&&r.ngZoneEventCoalescing||!1,ngZoneRunCoalescing:r&&r.ngZoneRunCoalescing||!1}),l=[{provide:Le,useValue:a}];return a.run(()=>{const c=pe.create({providers:l,parent:this.injector,name:n.moduleType.name}),u=n.create(c),d=u.injector.get(ir,null);if(!d)throw new Error("No ErrorHandler. Is platform module (BrowserModule) included?");return a.runOutsideAngular(()=>{const f=a.onError.subscribe({next:h=>{d.handleError(h)}});u.onDestroy(()=>{Tu(this._modules,u),f.unsubscribe()})}),function(e,t,n){try{const r=n();return Vs(r)?r.catch(o=>{throw t.runOutsideAngular(()=>e.handleError(o)),o}):r}catch(r){throw t.runOutsideAngular(()=>e.handleError(r)),r}}(d,a,()=>{const f=u.injector.get(go);return f.runInitializers(),f.donePromise.then(()=>(Vc(u.injector.get(Hn,Bs)||Bs),this._moduleDoBootstrap(u),u))})})}bootstrapModule(n,r=[]){const o=ly({},r);return function(e,t,n){const r=new ou(n);return Promise.resolve(r)}(0,0,n).then(i=>this.bootstrapModuleFactory(i,o))}_moduleDoBootstrap(n){const r=n.injector.get(wi);if(n._bootstrapComponents.length>0)n._bootstrapComponents.forEach(o=>r.bootstrap(o));else{if(!n.instance.ngDoBootstrap)throw new Error(`The module ${W(n.instance.constructor)} was bootstrapped, but it does not declare "@NgModule.bootstrap" components nor a "ngDoBootstrap" method. Please define one of these.`);n.instance.ngDoBootstrap(r)}this._modules.push(n)}onDestroy(n){this._destroyListeners.push(n)}get injector(){return this._injector}destroy(){if(this._destroyed)throw new Error("The platform has already been destroyed!");this._modules.slice().forEach(n=>n.destroy()),this._destroyListeners.forEach(n=>n()),this._destroyed=!0}get destroyed(){return this._destroyed}}return e.\u0275fac=function(n){return new(n||e)(Y(pe))},e.\u0275prov=te({token:e,factory:e.\u0275fac}),e})();function ly(e,t){return Array.isArray(t)?t.reduce(ly,e):Object.assign(Object.assign({},e),t)}let wi=(()=>{class e{constructor(n,r,o,i,s){this._zone=n,this._injector=r,this._exceptionHandler=o,this._componentFactoryResolver=i,this._initStatus=s,this._bootstrapListeners=[],this._views=[],this._runningTick=!1,this._stable=!0,this.componentTypes=[],this.components=[],this._onMicrotaskEmptySubscription=this._zone.onMicrotaskEmpty.subscribe({next:()=>{this._zone.run(()=>{this.tick()})}});const a=new qe(c=>{this._stable=this._zone.isStable&&!this._zone.hasPendingMacrotasks&&!this._zone.hasPendingMicrotasks,this._zone.runOutsideAngular(()=>{c.next(this._stable),c.complete()})}),l=new qe(c=>{let u;this._zone.runOutsideAngular(()=>{u=this._zone.onStable.subscribe(()=>{Le.assertNotInAngularZone(),bu(()=>{!this._stable&&!this._zone.hasPendingMacrotasks&&!this._zone.hasPendingMicrotasks&&(this._stable=!0,c.next(!0))})})});const d=this._zone.onUnstable.subscribe(()=>{Le.assertInAngularZone(),this._stable&&(this._stable=!1,this._zone.runOutsideAngular(()=>{c.next(!1)}))});return()=>{u.unsubscribe(),d.unsubscribe()}});this.isStable=function(...e){let t=Number.POSITIVE_INFINITY,n=null,r=e[e.length-1];return function(e){return e&&"function"==typeof e.schedule}(r)?(n=e.pop(),e.length>1&&"number"==typeof e[e.length-1]&&(t=e.pop())):"number"==typeof r&&(t=e.pop()),null===n&&1===e.length&&e[0]instanceof qe?e[0]:Fv(t)(function(e,t){return t?ef(e,t):new qe(Yd(e))}(e,n))}(a,l.pipe(e=>nf()(function(e,t){return function(r){let o;o="function"==typeof e?e:function(){return e};const i=Object.create(r,Bv);return i.source=r,i.subjectFactory=o,i}}(Gv)(e))))}bootstrap(n,r){if(!this._initStatus.done)throw new Error("Cannot bootstrap as there are still asynchronous initializers running. Bootstrap components in the `ngDoBootstrap` method of the root module.");let o;o=n instanceof Vm?n:this._componentFactoryResolver.resolveComponentFactory(n),this.componentTypes.push(o.componentType);const i=function(e){return e.isBoundToModule}(o)?void 0:this._injector.get(ur),a=o.create(pe.NULL,[],r||o.selector,i),l=a.location.nativeElement,c=a.injector.get(Iu,null),u=c&&a.injector.get(ey);return c&&u&&u.registerApplication(l,c),a.onDestroy(()=>{this.detachView(a.hostView),Tu(this.components,a),u&&u.unregisterApplication(l)}),this._loadComponent(a),a}tick(){if(this._runningTick)throw new Error("ApplicationRef.tick is called recursively");try{this._runningTick=!0;for(let n of this._views)n.detectChanges()}catch(n){this._zone.runOutsideAngular(()=>this._exceptionHandler.handleError(n))}finally{this._runningTick=!1}}attachView(n){const r=n;this._views.push(r),r.attachToAppRef(this)}detachView(n){const r=n;Tu(this._views,r),r.detachFromAppRef()}_loadComponent(n){this.attachView(n.hostView),this.tick(),this.components.push(n),this._injector.get(oA,[]).concat(this._bootstrapListeners).forEach(o=>o(n))}ngOnDestroy(){this._views.slice().forEach(n=>n.destroy()),this._onMicrotaskEmptySubscription.unsubscribe()}get viewCount(){return this._views.length}}return e.\u0275fac=function(n){return new(n||e)(Y(Le),Y(pe),Y(ir),Y(io),Y(go))},e.\u0275prov=te({token:e,factory:e.\u0275fac}),e})();function Tu(e,t){const n=e.indexOf(t);n>-1&&e.splice(n,1)}const WA=iy(null,"core",[{provide:Cu,useValue:"unknown"},{provide:ay,deps:[pe]},{provide:ey,deps:[]},{provide:vu,deps:[]}]),ZA=[{provide:wi,useClass:wi,deps:[Le,pe,ir,io,go]},{provide:ZM,deps:[Le],useFactory:function(e){let t=[];return e.onStable.subscribe(()=>{for(;t.length;)t.pop()()}),function(n){t.push(n)}}},{provide:go,useClass:go,deps:[[new en,na]]},{provide:oa,useClass:oa,deps:[]},rA,{provide:ui,useFactory:function(){return G1},deps:[]},{provide:ao,useFactory:function(){return z1},deps:[]},{provide:Hn,useFactory:function(e){return Vc(e=e||"undefined"!=typeof $localize&&$localize.locale||Bs),e},deps:[[new Uo(Hn),new en,new rr]]},{provide:Y_,useValue:"USD"}];let XA=(()=>{class e{constructor(n){}}return e.\u0275fac=function(n){return new(n||e)(Y(wi))},e.\u0275mod=mn({type:e}),e.\u0275inj=Ft({providers:ZA}),e})(),pa=null;function gr(){return pa}const nt=new X("DocumentToken");var Te=(()=>((Te=Te||{})[Te.Zero=0]="Zero",Te[Te.One=1]="One",Te[Te.Two=2]="Two",Te[Te.Few=3]="Few",Te[Te.Many=4]="Many",Te[Te.Other=5]="Other",Te))();const ax=function(e){return function(e){const t=function(e){return e.toLowerCase().replace(/_/g,"-")}(e);let n=um(t);if(n)return n;const r=t.split("-")[0];if(n=um(r),n)return n;if("en"===r)return DI;throw new Error(`Missing locale data for the locale "${e}".`)}(e)[A.PluralCase]};class wa{}let Vx=(()=>{class e extends wa{constructor(n){super(),this.locale=n}getPluralCategory(n,r){switch(ax(r||this.locale)(n)){case Te.Zero:return"zero";case Te.One:return"one";case Te.Two:return"two";case Te.Few:return"few";case Te.Many:return"many";default:return"other"}}}return e.\u0275fac=function(n){return new(n||e)(Y(Hn))},e.\u0275prov=te({token:e,factory:e.\u0275fac}),e})(),Ni=(()=>{class e{constructor(n,r,o,i){this._iterableDiffers=n,this._keyValueDiffers=r,this._ngEl=o,this._renderer=i,this._iterableDiffer=null,this._keyValueDiffer=null,this._initialClasses=[],this._rawClass=null}set klass(n){this._removeClasses(this._initialClasses),this._initialClasses="string"==typeof n?n.split(/\s+/):[],this._applyClasses(this._initialClasses),this._applyClasses(this._rawClass)}set ngClass(n){this._removeClasses(this._rawClass),this._applyClasses(this._initialClasses),this._iterableDiffer=null,this._keyValueDiffer=null,this._rawClass="string"==typeof n?n.split(/\s+/):n,this._rawClass&&(ni(this._rawClass)?this._iterableDiffer=this._iterableDiffers.find(this._rawClass).create():this._keyValueDiffer=this._keyValueDiffers.find(this._rawClass).create())}ngDoCheck(){if(this._iterableDiffer){const n=this._iterableDiffer.diff(this._rawClass);n&&this._applyIterableChanges(n)}else if(this._keyValueDiffer){const n=this._keyValueDiffer.diff(this._rawClass);n&&this._applyKeyValueChanges(n)}}_applyKeyValueChanges(n){n.forEachAddedItem(r=>this._toggleClass(r.key,r.currentValue)),n.forEachChangedItem(r=>this._toggleClass(r.key,r.currentValue)),n.forEachRemovedItem(r=>{r.previousValue&&this._toggleClass(r.key,!1)})}_applyIterableChanges(n){n.forEachAddedItem(r=>{if("string"!=typeof r.item)throw new Error(`NgClass can only toggle CSS classes expressed as strings, got ${W(r.item)}`);this._toggleClass(r.item,!0)}),n.forEachRemovedItem(r=>this._toggleClass(r.item,!1))}_applyClasses(n){n&&(Array.isArray(n)||n instanceof Set?n.forEach(r=>this._toggleClass(r,!0)):Object.keys(n).forEach(r=>this._toggleClass(r,!!n[r])))}_removeClasses(n){n&&(Array.isArray(n)||n instanceof Set?n.forEach(r=>this._toggleClass(r,!1)):Object.keys(n).forEach(r=>this._toggleClass(r,!1)))}_toggleClass(n,r){(n=n.trim())&&n.split(/\s+/g).forEach(o=>{r?this._renderer.addClass(this._ngEl.nativeElement,o):this._renderer.removeClass(this._ngEl.nativeElement,o)})}}return e.\u0275fac=function(n){return new(n||e)(I(ui),I(ao),I($e),I(cr))},e.\u0275dir=L({type:e,selectors:[["","ngClass",""]],inputs:{klass:["class","klass"],ngClass:"ngClass"}}),e})();class Bx{constructor(t,n,r,o){this.$implicit=t,this.ngForOf=n,this.index=r,this.count=o}get first(){return 0===this.index}get last(){return this.index===this.count-1}get even(){return this.index%2==0}get odd(){return!this.even}}let Yu=(()=>{class e{constructor(n,r,o){this._viewContainer=n,this._template=r,this._differs=o,this._ngForOf=null,this._ngForOfDirty=!0,this._differ=null}set ngForOf(n){this._ngForOf=n,this._ngForOfDirty=!0}set ngForTrackBy(n){this._trackByFn=n}get ngForTrackBy(){return this._trackByFn}set ngForTemplate(n){n&&(this._template=n)}ngDoCheck(){if(this._ngForOfDirty){this._ngForOfDirty=!1;const n=this._ngForOf;if(!this._differ&&n)try{this._differ=this._differs.find(n).create(this.ngForTrackBy)}catch(r){throw new Error(`Cannot find a differ supporting object '${n}' of type '${function(e){return e.name||typeof e}(n)}'. NgFor only supports binding to Iterables such as Arrays.`)}}if(this._differ){const n=this._differ.diff(this._ngForOf);n&&this._applyChanges(n)}}_applyChanges(n){const r=[];n.forEachOperation((o,i,s)=>{if(null==o.previousIndex){const a=this._viewContainer.createEmbeddedView(this._template,new Bx(null,this._ngForOf,-1,-1),null===s?void 0:s),l=new Wy(o,a);r.push(l)}else if(null==s)this._viewContainer.remove(null===i?void 0:i);else if(null!==i){const a=this._viewContainer.get(i);this._viewContainer.move(a,s);const l=new Wy(o,a);r.push(l)}});for(let o=0;o<r.length;o++)this._perViewChange(r[o].view,r[o].record);for(let o=0,i=this._viewContainer.length;o<i;o++){const s=this._viewContainer.get(o);s.context.index=o,s.context.count=i,s.context.ngForOf=this._ngForOf}n.forEachIdentityChange(o=>{this._viewContainer.get(o.currentIndex).context.$implicit=o.item})}_perViewChange(n,r){n.context.$implicit=r.item}static ngTemplateContextGuard(n,r){return!0}}return e.\u0275fac=function(n){return new(n||e)(I(dn),I(bn),I(ui))},e.\u0275dir=L({type:e,selectors:[["","ngFor","","ngForOf",""]],inputs:{ngForOf:"ngForOf",ngForTrackBy:"ngForTrackBy",ngForTemplate:"ngForTemplate"}}),e})();class Wy{constructor(t,n){this.record=t,this.view=n}}let yo=(()=>{class e{constructor(n,r){this._viewContainer=n,this._context=new jx,this._thenTemplateRef=null,this._elseTemplateRef=null,this._thenViewRef=null,this._elseViewRef=null,this._thenTemplateRef=r}set ngIf(n){this._context.$implicit=this._context.ngIf=n,this._updateView()}set ngIfThen(n){qy("ngIfThen",n),this._thenTemplateRef=n,this._thenViewRef=null,this._updateView()}set ngIfElse(n){qy("ngIfElse",n),this._elseTemplateRef=n,this._elseViewRef=null,this._updateView()}_updateView(){this._context.$implicit?this._thenViewRef||(this._viewContainer.clear(),this._elseViewRef=null,this._thenTemplateRef&&(this._thenViewRef=this._viewContainer.createEmbeddedView(this._thenTemplateRef,this._context))):this._elseViewRef||(this._viewContainer.clear(),this._thenViewRef=null,this._elseTemplateRef&&(this._elseViewRef=this._viewContainer.createEmbeddedView(this._elseTemplateRef,this._context)))}static ngTemplateContextGuard(n,r){return!0}}return e.\u0275fac=function(n){return new(n||e)(I(dn),I(bn))},e.\u0275dir=L({type:e,selectors:[["","ngIf",""]],inputs:{ngIf:"ngIf",ngIfThen:"ngIfThen",ngIfElse:"ngIfElse"}}),e})();class jx{constructor(){this.$implicit=null,this.ngIf=null}}function qy(e,t){if(t&&!t.createEmbeddedView)throw new Error(`${e} must be a TemplateRef, but received '${W(t)}'.`)}let Yy=(()=>{class e{transform(n,r,o){if(null==n)return null;if(!this.supports(n))throw function(e,t){return Error(`InvalidPipeArgument: '${t}' for pipe '${W(e)}'`)}(e,n);return n.slice(r,o)}supports(n){return"string"==typeof n||Array.isArray(n)}}return e.\u0275fac=function(n){return new(n||e)},e.\u0275pipe=ot({name:"slice",type:e,pure:!1}),e})(),fN=(()=>{class e{}return e.\u0275fac=function(n){return new(n||e)},e.\u0275mod=mn({type:e}),e.\u0275inj=Ft({providers:[{provide:wa,useClass:Vx}]}),e})();class td extends class extends class{}{constructor(){super(...arguments),this.supportsDOMEvents=!0}}{static makeCurrent(){!function(e){pa||(pa=e)}(new td)}onAndCancel(t,n,r){return t.addEventListener(n,r,!1),()=>{t.removeEventListener(n,r,!1)}}dispatchEvent(t,n){t.dispatchEvent(n)}remove(t){t.parentNode&&t.parentNode.removeChild(t)}createElement(t,n){return(n=n||this.getDefaultDocument()).createElement(t)}createHtmlDocument(){return document.implementation.createHTMLDocument("fakeTitle")}getDefaultDocument(){return document}isElementNode(t){return t.nodeType===Node.ELEMENT_NODE}isShadowRoot(t){return t instanceof DocumentFragment}getGlobalEventTarget(t,n){return"window"===n?window:"document"===n?t:"body"===n?t.body:null}getBaseHref(t){const n=(Ri=Ri||document.querySelector("base"),Ri?Ri.getAttribute("href"):null);return null==n?null:function(e){Ia=Ia||document.createElement("a"),Ia.setAttribute("href",e);const t=Ia.pathname;return"/"===t.charAt(0)?t:`/${t}`}(n)}resetBaseElement(){Ri=null}getUserAgent(){return window.navigator.userAgent}getCookie(t){return function(e,t){t=encodeURIComponent(t);for(const n of e.split(";")){const r=n.indexOf("="),[o,i]=-1==r?[n,""]:[n.slice(0,r),n.slice(r+1)];if(o.trim()===t)return decodeURIComponent(i)}return null}(document.cookie,t)}}let Ia,Ri=null;const Xy=new X("TRANSITION_ID"),bN=[{provide:na,useFactory:function(e,t,n){return()=>{n.get(go).donePromise.then(()=>{const r=gr(),o=t.querySelectorAll(`style[ng-transition="${e}"]`);for(let i=0;i<o.length;i++)r.remove(o[i])})}},deps:[Xy,nt,pe],multi:!0}];class nd{static init(){!function(e){Mu=e}(new nd)}addToWindow(t){ne.getAngularTestability=(r,o=!0)=>{const i=t.findTestabilityInTree(r,o);if(null==i)throw new Error("Could not find testability for element.");return i},ne.getAllAngularTestabilities=()=>t.getAllTestabilities(),ne.getAllAngularRootElements=()=>t.getAllRootElements(),ne.frameworkStabilizers||(ne.frameworkStabilizers=[]),ne.frameworkStabilizers.push(r=>{const o=ne.getAllAngularTestabilities();let i=o.length,s=!1;const a=function(l){s=s||l,i--,0==i&&r(s)};o.forEach(function(l){l.whenStable(a)})})}findTestabilityInTree(t,n,r){if(null==n)return null;const o=t.getTestability(n);return null!=o?o:r?gr().isShadowRoot(n)?this.findTestabilityInTree(t,n.host,!0):this.findTestabilityInTree(t,n.parentElement,!0):null}}let EN=(()=>{class e{build(){return new XMLHttpRequest}}return e.\u0275fac=function(n){return new(n||e)},e.\u0275prov=te({token:e,factory:e.\u0275fac}),e})();const Fi=new X("EventManagerPlugins");let Ta=(()=>{class e{constructor(n,r){this._zone=r,this._eventNameToPlugin=new Map,n.forEach(o=>o.manager=this),this._plugins=n.slice().reverse()}addEventListener(n,r,o){return this._findPluginFor(r).addEventListener(n,r,o)}addGlobalEventListener(n,r,o){return this._findPluginFor(r).addGlobalEventListener(n,r,o)}getZone(){return this._zone}_findPluginFor(n){const r=this._eventNameToPlugin.get(n);if(r)return r;const o=this._plugins;for(let i=0;i<o.length;i++){const s=o[i];if(s.supports(n))return this._eventNameToPlugin.set(n,s),s}throw new Error(`No event manager plugin found for event ${n}`)}}return e.\u0275fac=function(n){return new(n||e)(Y(Fi),Y(Le))},e.\u0275prov=te({token:e,factory:e.\u0275fac}),e})();class rd{constructor(t){this._doc=t}addGlobalEventListener(t,n,r){const o=gr().getGlobalEventTarget(this._doc,t);if(!o)throw new Error(`Unsupported event target ${o} for event ${n}`);return this.addEventListener(o,n,r)}}let tC=(()=>{class e{constructor(){this._stylesSet=new Set}addStyles(n){const r=new Set;n.forEach(o=>{this._stylesSet.has(o)||(this._stylesSet.add(o),r.add(o))}),this.onStylesAdded(r)}onStylesAdded(n){}getAllStyles(){return Array.from(this._stylesSet)}}return e.\u0275fac=function(n){return new(n||e)},e.\u0275prov=te({token:e,factory:e.\u0275fac}),e})(),Oi=(()=>{class e extends tC{constructor(n){super(),this._doc=n,this._hostNodes=new Map,this._hostNodes.set(n.head,[])}_addStylesToHost(n,r,o){n.forEach(i=>{const s=this._doc.createElement("style");s.textContent=i,o.push(r.appendChild(s))})}addHost(n){const r=[];this._addStylesToHost(this._stylesSet,n,r),this._hostNodes.set(n,r)}removeHost(n){const r=this._hostNodes.get(n);r&&r.forEach(nC),this._hostNodes.delete(n)}onStylesAdded(n){this._hostNodes.forEach((r,o)=>{this._addStylesToHost(n,o,r)})}ngOnDestroy(){this._hostNodes.forEach(n=>n.forEach(nC))}}return e.\u0275fac=function(n){return new(n||e)(Y(nt))},e.\u0275prov=te({token:e,factory:e.\u0275fac}),e})();function nC(e){gr().remove(e)}const od={svg:"http://www.w3.org/2000/svg",xhtml:"http://www.w3.org/1999/xhtml",xlink:"http://www.w3.org/1999/xlink",xml:"http://www.w3.org/XML/1998/namespace",xmlns:"http://www.w3.org/2000/xmlns/"},id=/%COMP%/g;function Aa(e,t,n){for(let r=0;r<t.length;r++){let o=t[r];Array.isArray(o)?Aa(e,o,n):(o=o.replace(id,e),n.push(o))}return n}function iC(e){return t=>{if("__ngUnwrap__"===t)return e;!1===e(t)&&(t.preventDefault(),t.returnValue=!1)}}let sd=(()=>{class e{constructor(n,r,o){this.eventManager=n,this.sharedStylesHost=r,this.appId=o,this.rendererByCompId=new Map,this.defaultRenderer=new ad(n)}createRenderer(n,r){if(!n||!r)return this.defaultRenderer;switch(r.encapsulation){case Se.Emulated:{let o=this.rendererByCompId.get(r.id);return o||(o=new LN(this.eventManager,this.sharedStylesHost,r,this.appId),this.rendererByCompId.set(r.id,o)),o.applyToHost(n),o}case 1:case Se.ShadowDom:return new BN(this.eventManager,this.sharedStylesHost,n,r);default:if(!this.rendererByCompId.has(r.id)){const o=Aa(r.id,r.styles,[]);this.sharedStylesHost.addStyles(o),this.rendererByCompId.set(r.id,this.defaultRenderer)}return this.defaultRenderer}}begin(){}end(){}}return e.\u0275fac=function(n){return new(n||e)(Y(Ta),Y(Oi),Y(Ei))},e.\u0275prov=te({token:e,factory:e.\u0275fac}),e})();class ad{constructor(t){this.eventManager=t,this.data=Object.create(null)}destroy(){}createElement(t,n){return n?document.createElementNS(od[n]||n,t):document.createElement(t)}createComment(t){return document.createComment(t)}createText(t){return document.createTextNode(t)}appendChild(t,n){t.appendChild(n)}insertBefore(t,n,r){t&&t.insertBefore(n,r)}removeChild(t,n){t&&t.removeChild(n)}selectRootElement(t,n){let r="string"==typeof t?document.querySelector(t):t;if(!r)throw new Error(`The selector "${t}" did not match any elements`);return n||(r.textContent=""),r}parentNode(t){return t.parentNode}nextSibling(t){return t.nextSibling}setAttribute(t,n,r,o){if(o){n=o+":"+n;const i=od[o];i?t.setAttributeNS(i,n,r):t.setAttribute(n,r)}else t.setAttribute(n,r)}removeAttribute(t,n,r){if(r){const o=od[r];o?t.removeAttributeNS(o,n):t.removeAttribute(`${r}:${n}`)}else t.removeAttribute(n)}addClass(t,n){t.classList.add(n)}removeClass(t,n){t.classList.remove(n)}setStyle(t,n,r,o){o&(mt.DashCase|mt.Important)?t.style.setProperty(n,r,o&mt.Important?"important":""):t.style[n]=r}removeStyle(t,n,r){r&mt.DashCase?t.style.removeProperty(n):t.style[n]=""}setProperty(t,n,r){t[n]=r}setValue(t,n){t.nodeValue=n}listen(t,n,r){return"string"==typeof t?this.eventManager.addGlobalEventListener(t,n,iC(r)):this.eventManager.addEventListener(t,n,iC(r))}}class LN extends ad{constructor(t,n,r,o){super(t),this.component=r;const i=Aa(o+"-"+r.id,r.styles,[]);n.addStyles(i),this.contentAttr=function(e){return"_ngcontent-%COMP%".replace(id,e)}(o+"-"+r.id),this.hostAttr=function(e){return"_nghost-%COMP%".replace(id,e)}(o+"-"+r.id)}applyToHost(t){super.setAttribute(t,this.hostAttr,"")}createElement(t,n){const r=super.createElement(t,n);return super.setAttribute(r,this.contentAttr,""),r}}class BN extends ad{constructor(t,n,r,o){super(t),this.sharedStylesHost=n,this.hostEl=r,this.shadowRoot=r.attachShadow({mode:"open"}),this.sharedStylesHost.addHost(this.shadowRoot);const i=Aa(o.id,o.styles,[]);for(let s=0;s<i.length;s++){const a=document.createElement("style");a.textContent=i[s],this.shadowRoot.appendChild(a)}}nodeOrShadowRoot(t){return t===this.hostEl?this.shadowRoot:t}destroy(){this.sharedStylesHost.removeHost(this.shadowRoot)}appendChild(t,n){return super.appendChild(this.nodeOrShadowRoot(t),n)}insertBefore(t,n,r){return super.insertBefore(this.nodeOrShadowRoot(t),n,r)}removeChild(t,n){return super.removeChild(this.nodeOrShadowRoot(t),n)}parentNode(t){return this.nodeOrShadowRoot(super.parentNode(this.nodeOrShadowRoot(t)))}}let HN=(()=>{class e extends rd{constructor(n){super(n)}supports(n){return!0}addEventListener(n,r,o){return n.addEventListener(r,o,!1),()=>this.removeEventListener(n,r,o)}removeEventListener(n,r,o){return n.removeEventListener(r,o)}}return e.\u0275fac=function(n){return new(n||e)(Y(nt))},e.\u0275prov=te({token:e,factory:e.\u0275fac}),e})();const lC=["alt","control","meta","shift"],qN={"\b":"Backspace","\t":"Tab","\x7f":"Delete","\x1b":"Escape",Del:"Delete",Esc:"Escape",Left:"ArrowLeft",Right:"ArrowRight",Up:"ArrowUp",Down:"ArrowDown",Menu:"ContextMenu",Scroll:"ScrollLock",Win:"OS"},cC={A:"1",B:"2",C:"3",D:"4",E:"5",F:"6",G:"7",H:"8",I:"9",J:"*",K:"+",M:"-",N:".",O:"/","`":"0","\x90":"NumLock"},QN={alt:e=>e.altKey,control:e=>e.ctrlKey,meta:e=>e.metaKey,shift:e=>e.shiftKey};let KN=(()=>{class e extends rd{constructor(n){super(n)}supports(n){return null!=e.parseEventName(n)}addEventListener(n,r,o){const i=e.parseEventName(r),s=e.eventCallback(i.fullKey,o,this.manager.getZone());return this.manager.getZone().runOutsideAngular(()=>gr().onAndCancel(n,i.domEventName,s))}static parseEventName(n){const r=n.toLowerCase().split("."),o=r.shift();if(0===r.length||"keydown"!==o&&"keyup"!==o)return null;const i=e._normalizeKey(r.pop());let s="";if(lC.forEach(l=>{const c=r.indexOf(l);c>-1&&(r.splice(c,1),s+=l+".")}),s+=i,0!=r.length||0===i.length)return null;const a={};return a.domEventName=o,a.fullKey=s,a}static getEventFullKey(n){let r="",o=function(e){let t=e.key;if(null==t){if(t=e.keyIdentifier,null==t)return"Unidentified";t.startsWith("U+")&&(t=String.fromCharCode(parseInt(t.substring(2),16)),3===e.location&&cC.hasOwnProperty(t)&&(t=cC[t]))}return qN[t]||t}(n);return o=o.toLowerCase()," "===o?o="space":"."===o&&(o="dot"),lC.forEach(i=>{i!=o&&QN[i](n)&&(r+=i+".")}),r+=o,r}static eventCallback(n,r,o){return i=>{e.getEventFullKey(i)===n&&o.runGuarded(()=>r(i))}}static _normalizeKey(n){switch(n){case"esc":return"escape";default:return n}}}return e.\u0275fac=function(n){return new(n||e)(Y(nt))},e.\u0275prov=te({token:e,factory:e.\u0275fac}),e})();const rR=iy(WA,"browser",[{provide:Cu,useValue:"browser"},{provide:K_,useValue:function(){td.makeCurrent(),nd.init()},multi:!0},{provide:nt,useFactory:function(){return function(e){il=e}(document),document},deps:[]}]),oR=[[],{provide:Xo,useValue:"root"},{provide:ir,useFactory:function(){return new ir},deps:[]},{provide:Fi,useClass:HN,multi:!0,deps:[nt,Le,Cu]},{provide:Fi,useClass:KN,multi:!0,deps:[nt]},[],{provide:sd,useClass:sd,deps:[Ta,Oi,Ei]},{provide:zs,useExisting:sd},{provide:tC,useExisting:Oi},{provide:Oi,useClass:Oi,deps:[nt]},{provide:Iu,useClass:Iu,deps:[Le]},{provide:Ta,useClass:Ta,deps:[Fi,Le]},{provide:class{},useClass:EN,deps:[]},[]];let iR=(()=>{class e{constructor(n){if(n)throw new Error("BrowserModule has already been loaded. If you need access to common directives such as NgIf and NgFor from a lazy loaded module, import CommonModule instead.")}static withServerTransition(n){return{ngModule:e,providers:[{provide:Ei,useValue:n.appId},{provide:Xy,useExisting:Ei},bN]}}}return e.\u0275fac=function(n){return new(n||e)(Y(e,12))},e.\u0275mod=mn({type:e}),e.\u0275inj=Ft({providers:oR,imports:[fN,XA]}),e})();function Sa(e,t){return new qe(n=>{const r=e.length;if(0===r)return void n.complete();const o=new Array(r);let i=0,s=0;for(let a=0;a<r;a++){const l=Wa(e[a]);let c=!1;n.add(l.subscribe({next:u=>{c||(c=!0,s++),o[a]=u},error:u=>n.error(u),complete:()=>{i++,(i===r||!c)&&(s===r&&n.next(t?t.reduce((u,d,f)=>(u[d]=o[f],u),{}):o),n.complete())}}))}})}"undefined"!=typeof window&&window;let dC=(()=>{class e{constructor(n,r){this._renderer=n,this._elementRef=r,this.onChange=o=>{},this.onTouched=()=>{}}setProperty(n,r){this._renderer.setProperty(this._elementRef.nativeElement,n,r)}registerOnTouched(n){this.onTouched=n}registerOnChange(n){this.onChange=n}setDisabledState(n){this.setProperty("disabled",n)}}return e.\u0275fac=function(n){return new(n||e)(I(cr),I($e))},e.\u0275dir=L({type:e}),e})(),mr=(()=>{class e extends dC{}return e.\u0275fac=function(){let t;return function(r){return(t||(t=Et(e)))(r||e)}}(),e.\u0275dir=L({type:e,features:[ge]}),e})();const fn=new X("NgValueAccessor"),gR={provide:fn,useExisting:ue(()=>Pi),multi:!0},_R=new X("CompositionEventMode");let Pi=(()=>{class e extends dC{constructor(n,r,o){super(n,r),this._compositionMode=o,this._composing=!1,null==this._compositionMode&&(this._compositionMode=!function(){const e=gr()?gr().getUserAgent():"";return/android (\d+)/.test(e.toLowerCase())}())}writeValue(n){this.setProperty("value",null==n?"":n)}_handleInput(n){(!this._compositionMode||this._compositionMode&&!this._composing)&&this.onChange(n)}_compositionStart(){this._composing=!0}_compositionEnd(n){this._composing=!1,this._compositionMode&&this.onChange(n)}}return e.\u0275fac=function(n){return new(n||e)(I(cr),I($e),I(_R,8))},e.\u0275dir=L({type:e,selectors:[["input","formControlName","",3,"type","checkbox"],["textarea","formControlName",""],["input","formControl","",3,"type","checkbox"],["textarea","formControl",""],["input","ngModel","",3,"type","checkbox"],["textarea","ngModel",""],["","ngDefaultControl",""]],hostBindings:function(n,r){1&n&&Z("input",function(i){return r._handleInput(i.target.value)})("blur",function(){return r.onTouched()})("compositionstart",function(){return r._compositionStart()})("compositionend",function(i){return r._compositionEnd(i.target.value)})},features:[_e([gR]),ge]}),e})();const We=new X("NgValidators"),Gn=new X("NgAsyncValidators");function bC(e){return null!=e}function EC(e){const t=Vs(e)?Wa(e):e;return Rc(t),t}function wC(e){let t={};return e.forEach(n=>{t=null!=n?Object.assign(Object.assign({},t),n):t}),0===Object.keys(t).length?null:t}function IC(e,t){return t.map(n=>n(e))}function MC(e){return e.map(t=>function(e){return!e.validate}(t)?t:n=>t.validate(n))}function fd(e){return null!=e?function(e){if(!e)return null;const t=e.filter(bC);return 0==t.length?null:function(n){return wC(IC(n,t))}}(MC(e)):null}function hd(e){return null!=e?function(e){if(!e)return null;const t=e.filter(bC);return 0==t.length?null:function(n){return function(...e){if(1===e.length){const t=e[0];if($a(t))return Sa(t,null);if(Ua(t)&&Object.getPrototypeOf(t)===Object.prototype){const n=Object.keys(t);return Sa(n.map(r=>t[r]),n)}}if("function"==typeof e[e.length-1]){const t=e.pop();return Sa(e=1===e.length&&$a(e[0])?e[0]:e,null).pipe(za(n=>t(...n)))}return Sa(e,null)}(IC(n,t).map(EC)).pipe(za(wC))}}(MC(e)):null}function SC(e,t){return null===e?[t]:Array.isArray(e)?[...e,t]:[e,t]}function pd(e){return e?Array.isArray(e)?e:[e]:[]}function xa(e,t){return Array.isArray(e)?e.includes(t):e===t}function RC(e,t){const n=pd(t);return pd(e).forEach(o=>{xa(n,o)||n.push(o)}),n}function FC(e,t){return pd(t).filter(n=>!xa(e,n))}let OC=(()=>{class e{constructor(){this._rawValidators=[],this._rawAsyncValidators=[],this._onDestroyCallbacks=[]}get value(){return this.control?this.control.value:null}get valid(){return this.control?this.control.valid:null}get invalid(){return this.control?this.control.invalid:null}get pending(){return this.control?this.control.pending:null}get disabled(){return this.control?this.control.disabled:null}get enabled(){return this.control?this.control.enabled:null}get errors(){return this.control?this.control.errors:null}get pristine(){return this.control?this.control.pristine:null}get dirty(){return this.control?this.control.dirty:null}get touched(){return this.control?this.control.touched:null}get status(){return this.control?this.control.status:null}get untouched(){return this.control?this.control.untouched:null}get statusChanges(){return this.control?this.control.statusChanges:null}get valueChanges(){return this.control?this.control.valueChanges:null}get path(){return null}_setValidators(n){this._rawValidators=n||[],this._composedValidatorFn=fd(this._rawValidators)}_setAsyncValidators(n){this._rawAsyncValidators=n||[],this._composedAsyncValidatorFn=hd(this._rawAsyncValidators)}get validator(){return this._composedValidatorFn||null}get asyncValidator(){return this._composedAsyncValidatorFn||null}_registerOnDestroy(n){this._onDestroyCallbacks.push(n)}_invokeOnDestroyCallbacks(){this._onDestroyCallbacks.forEach(n=>n()),this._onDestroyCallbacks=[]}reset(n){this.control&&this.control.reset(n)}hasError(n,r){return!!this.control&&this.control.hasError(n,r)}getError(n,r){return this.control?this.control.getError(n,r):null}}return e.\u0275fac=function(n){return new(n||e)},e.\u0275dir=L({type:e}),e})(),rt=(()=>{class e extends OC{get formDirective(){return null}get path(){return null}}return e.\u0275fac=function(){let t;return function(r){return(t||(t=Et(e)))(r||e)}}(),e.\u0275dir=L({type:e,features:[ge]}),e})();class Wn extends OC{constructor(){super(...arguments),this._parent=null,this.name=null,this.valueAccessor=null}}let gd=(()=>{class e extends class{constructor(t){this._cd=t}is(t){var n,r,o;return"submitted"===t?!!(null===(n=this._cd)||void 0===n?void 0:n.submitted):!!(null===(o=null===(r=this._cd)||void 0===r?void 0:r.control)||void 0===o?void 0:o[t])}}{constructor(n){super(n)}}return e.\u0275fac=function(n){return new(n||e)(I(Wn,2))},e.\u0275dir=L({type:e,selectors:[["","formControlName",""],["","ngModel",""],["","formControl",""]],hostVars:14,hostBindings:function(n,r){2&n&&ks("ng-untouched",r.is("untouched"))("ng-touched",r.is("touched"))("ng-pristine",r.is("pristine"))("ng-dirty",r.is("dirty"))("ng-valid",r.is("valid"))("ng-invalid",r.is("invalid"))("ng-pending",r.is("pending"))},features:[ge]}),e})();function Vi(e,t){(function(e,t){const n=function(e){return e._rawValidators}(e);null!==t.validator?e.setValidators(SC(n,t.validator)):"function"==typeof n&&e.setValidators([n]);const r=function(e){return e._rawAsyncValidators}(e);null!==t.asyncValidator?e.setAsyncValidators(SC(r,t.asyncValidator)):"function"==typeof r&&e.setAsyncValidators([r]);const o=()=>e.updateValueAndValidity();Oa(t._rawValidators,o),Oa(t._rawAsyncValidators,o)})(e,t),t.valueAccessor.writeValue(e.value),function(e,t){t.valueAccessor.registerOnChange(n=>{e._pendingValue=n,e._pendingChange=!0,e._pendingDirty=!0,"change"===e.updateOn&&VC(e,t)})}(e,t),function(e,t){const n=(r,o)=>{t.valueAccessor.writeValue(r),o&&t.viewToModelUpdate(r)};e.registerOnChange(n),t._registerOnDestroy(()=>{e._unregisterOnChange(n)})}(e,t),function(e,t){t.valueAccessor.registerOnTouched(()=>{e._pendingTouched=!0,"blur"===e.updateOn&&e._pendingChange&&VC(e,t),"submit"!==e.updateOn&&e.markAsTouched()})}(e,t),function(e,t){if(t.valueAccessor.setDisabledState){const n=r=>{t.valueAccessor.setDisabledState(r)};e.registerOnDisabledChange(n),t._registerOnDestroy(()=>{e._unregisterOnDisabledChange(n)})}}(e,t)}function Oa(e,t){e.forEach(n=>{n.registerOnValidatorChange&&n.registerOnValidatorChange(t)})}function VC(e,t){e._pendingDirty&&e.markAsDirty(),e.setValue(e._pendingValue,{emitModelToViewChange:!1}),t.viewToModelUpdate(e._pendingValue),e._pendingChange=!1}function Va(e,t){const n=e.indexOf(t);n>-1&&e.splice(n,1)}const ki="VALID",ka="INVALID",Co="PENDING",Li="DISABLED";function Dd(e){return(Ed(e)?e.validators:e)||null}function BC(e){return Array.isArray(e)?fd(e):e||null}function bd(e,t){return(Ed(t)?t.asyncValidators:e)||null}function HC(e){return Array.isArray(e)?hd(e):e||null}function Ed(e){return null!=e&&!Array.isArray(e)&&"object"==typeof e}class wd{constructor(t,n){this._hasOwnPendingAsyncValidator=!1,this._onCollectionChange=()=>{},this._parent=null,this.pristine=!0,this.touched=!1,this._onDisabledChange=[],this._rawValidators=t,this._rawAsyncValidators=n,this._composedValidatorFn=BC(this._rawValidators),this._composedAsyncValidatorFn=HC(this._rawAsyncValidators)}get validator(){return this._composedValidatorFn}set validator(t){this._rawValidators=this._composedValidatorFn=t}get asyncValidator(){return this._composedAsyncValidatorFn}set asyncValidator(t){this._rawAsyncValidators=this._composedAsyncValidatorFn=t}get parent(){return this._parent}get valid(){return this.status===ki}get invalid(){return this.status===ka}get pending(){return this.status==Co}get disabled(){return this.status===Li}get enabled(){return this.status!==Li}get dirty(){return!this.pristine}get untouched(){return!this.touched}get updateOn(){return this._updateOn?this._updateOn:this.parent?this.parent.updateOn:"change"}setValidators(t){this._rawValidators=t,this._composedValidatorFn=BC(t)}setAsyncValidators(t){this._rawAsyncValidators=t,this._composedAsyncValidatorFn=HC(t)}addValidators(t){this.setValidators(RC(t,this._rawValidators))}addAsyncValidators(t){this.setAsyncValidators(RC(t,this._rawAsyncValidators))}removeValidators(t){this.setValidators(FC(t,this._rawValidators))}removeAsyncValidators(t){this.setAsyncValidators(FC(t,this._rawAsyncValidators))}hasValidator(t){return xa(this._rawValidators,t)}hasAsyncValidator(t){return xa(this._rawAsyncValidators,t)}clearValidators(){this.validator=null}clearAsyncValidators(){this.asyncValidator=null}markAsTouched(t={}){this.touched=!0,this._parent&&!t.onlySelf&&this._parent.markAsTouched(t)}markAllAsTouched(){this.markAsTouched({onlySelf:!0}),this._forEachChild(t=>t.markAllAsTouched())}markAsUntouched(t={}){this.touched=!1,this._pendingTouched=!1,this._forEachChild(n=>{n.markAsUntouched({onlySelf:!0})}),this._parent&&!t.onlySelf&&this._parent._updateTouched(t)}markAsDirty(t={}){this.pristine=!1,this._parent&&!t.onlySelf&&this._parent.markAsDirty(t)}markAsPristine(t={}){this.pristine=!0,this._pendingDirty=!1,this._forEachChild(n=>{n.markAsPristine({onlySelf:!0})}),this._parent&&!t.onlySelf&&this._parent._updatePristine(t)}markAsPending(t={}){this.status=Co,!1!==t.emitEvent&&this.statusChanges.emit(this.status),this._parent&&!t.onlySelf&&this._parent.markAsPending(t)}disable(t={}){const n=this._parentMarkedDirty(t.onlySelf);this.status=Li,this.errors=null,this._forEachChild(r=>{r.disable(Object.assign(Object.assign({},t),{onlySelf:!0}))}),this._updateValue(),!1!==t.emitEvent&&(this.valueChanges.emit(this.value),this.statusChanges.emit(this.status)),this._updateAncestors(Object.assign(Object.assign({},t),{skipPristineCheck:n})),this._onDisabledChange.forEach(r=>r(!0))}enable(t={}){const n=this._parentMarkedDirty(t.onlySelf);this.status=ki,this._forEachChild(r=>{r.enable(Object.assign(Object.assign({},t),{onlySelf:!0}))}),this.updateValueAndValidity({onlySelf:!0,emitEvent:t.emitEvent}),this._updateAncestors(Object.assign(Object.assign({},t),{skipPristineCheck:n})),this._onDisabledChange.forEach(r=>r(!1))}_updateAncestors(t){this._parent&&!t.onlySelf&&(this._parent.updateValueAndValidity(t),t.skipPristineCheck||this._parent._updatePristine(),this._parent._updateTouched())}setParent(t){this._parent=t}updateValueAndValidity(t={}){this._setInitialStatus(),this._updateValue(),this.enabled&&(this._cancelExistingSubscription(),this.errors=this._runValidator(),this.status=this._calculateStatus(),(this.status===ki||this.status===Co)&&this._runAsyncValidator(t.emitEvent)),!1!==t.emitEvent&&(this.valueChanges.emit(this.value),this.statusChanges.emit(this.status)),this._parent&&!t.onlySelf&&this._parent.updateValueAndValidity(t)}_updateTreeValidity(t={emitEvent:!0}){this._forEachChild(n=>n._updateTreeValidity(t)),this.updateValueAndValidity({onlySelf:!0,emitEvent:t.emitEvent})}_setInitialStatus(){this.status=this._allControlsDisabled()?Li:ki}_runValidator(){return this.validator?this.validator(this):null}_runAsyncValidator(t){if(this.asyncValidator){this.status=Co,this._hasOwnPendingAsyncValidator=!0;const n=EC(this.asyncValidator(this));this._asyncValidationSubscription=n.subscribe(r=>{this._hasOwnPendingAsyncValidator=!1,this.setErrors(r,{emitEvent:t})})}}_cancelExistingSubscription(){this._asyncValidationSubscription&&(this._asyncValidationSubscription.unsubscribe(),this._hasOwnPendingAsyncValidator=!1)}setErrors(t,n={}){this.errors=t,this._updateControlsErrors(!1!==n.emitEvent)}get(t){return function(e,t,n){if(null==t||(Array.isArray(t)||(t=t.split(".")),Array.isArray(t)&&0===t.length))return null;let r=e;return t.forEach(o=>{r=r instanceof Id?r.controls.hasOwnProperty(o)?r.controls[o]:null:r instanceof RR&&r.at(o)||null}),r}(this,t)}getError(t,n){const r=n?this.get(n):this;return r&&r.errors?r.errors[t]:null}hasError(t,n){return!!this.getError(t,n)}get root(){let t=this;for(;t._parent;)t=t._parent;return t}_updateControlsErrors(t){this.status=this._calculateStatus(),t&&this.statusChanges.emit(this.status),this._parent&&this._parent._updateControlsErrors(t)}_initObservables(){this.valueChanges=new ze,this.statusChanges=new ze}_calculateStatus(){return this._allControlsDisabled()?Li:this.errors?ka:this._hasOwnPendingAsyncValidator||this._anyControlsHaveStatus(Co)?Co:this._anyControlsHaveStatus(ka)?ka:ki}_anyControlsHaveStatus(t){return this._anyControls(n=>n.status===t)}_anyControlsDirty(){return this._anyControls(t=>t.dirty)}_anyControlsTouched(){return this._anyControls(t=>t.touched)}_updatePristine(t={}){this.pristine=!this._anyControlsDirty(),this._parent&&!t.onlySelf&&this._parent._updatePristine(t)}_updateTouched(t={}){this.touched=this._anyControlsTouched(),this._parent&&!t.onlySelf&&this._parent._updateTouched(t)}_isBoxedValue(t){return"object"==typeof t&&null!==t&&2===Object.keys(t).length&&"value"in t&&"disabled"in t}_registerOnCollectionChange(t){this._onCollectionChange=t}_setUpdateStrategy(t){Ed(t)&&null!=t.updateOn&&(this._updateOn=t.updateOn)}_parentMarkedDirty(t){return!t&&!(!this._parent||!this._parent.dirty)&&!this._parent._anyControlsDirty()}}class La extends wd{constructor(t=null,n,r){super(Dd(n),bd(r,n)),this._onChange=[],this._applyFormState(t),this._setUpdateStrategy(n),this._initObservables(),this.updateValueAndValidity({onlySelf:!0,emitEvent:!!this.asyncValidator})}setValue(t,n={}){this.value=this._pendingValue=t,this._onChange.length&&!1!==n.emitModelToViewChange&&this._onChange.forEach(r=>r(this.value,!1!==n.emitViewToModelChange)),this.updateValueAndValidity(n)}patchValue(t,n={}){this.setValue(t,n)}reset(t=null,n={}){this._applyFormState(t),this.markAsPristine(n),this.markAsUntouched(n),this.setValue(this.value,n),this._pendingChange=!1}_updateValue(){}_anyControls(t){return!1}_allControlsDisabled(){return this.disabled}registerOnChange(t){this._onChange.push(t)}_unregisterOnChange(t){Va(this._onChange,t)}registerOnDisabledChange(t){this._onDisabledChange.push(t)}_unregisterOnDisabledChange(t){Va(this._onDisabledChange,t)}_forEachChild(t){}_syncPendingControls(){return!("submit"!==this.updateOn||(this._pendingDirty&&this.markAsDirty(),this._pendingTouched&&this.markAsTouched(),!this._pendingChange)||(this.setValue(this._pendingValue,{onlySelf:!0,emitModelToViewChange:!1}),0))}_applyFormState(t){this._isBoxedValue(t)?(this.value=this._pendingValue=t.value,t.disabled?this.disable({onlySelf:!0,emitEvent:!1}):this.enable({onlySelf:!0,emitEvent:!1})):this.value=this._pendingValue=t}}class Id extends wd{constructor(t,n,r){super(Dd(n),bd(r,n)),this.controls=t,this._initObservables(),this._setUpdateStrategy(n),this._setUpControls(),this.updateValueAndValidity({onlySelf:!0,emitEvent:!!this.asyncValidator})}registerControl(t,n){return this.controls[t]?this.controls[t]:(this.controls[t]=n,n.setParent(this),n._registerOnCollectionChange(this._onCollectionChange),n)}addControl(t,n,r={}){this.registerControl(t,n),this.updateValueAndValidity({emitEvent:r.emitEvent}),this._onCollectionChange()}removeControl(t,n={}){this.controls[t]&&this.controls[t]._registerOnCollectionChange(()=>{}),delete this.controls[t],this.updateValueAndValidity({emitEvent:n.emitEvent}),this._onCollectionChange()}setControl(t,n,r={}){this.controls[t]&&this.controls[t]._registerOnCollectionChange(()=>{}),delete this.controls[t],n&&this.registerControl(t,n),this.updateValueAndValidity({emitEvent:r.emitEvent}),this._onCollectionChange()}contains(t){return this.controls.hasOwnProperty(t)&&this.controls[t].enabled}setValue(t,n={}){this._checkAllValuesPresent(t),Object.keys(t).forEach(r=>{this._throwIfControlMissing(r),this.controls[r].setValue(t[r],{onlySelf:!0,emitEvent:n.emitEvent})}),this.updateValueAndValidity(n)}patchValue(t,n={}){null!=t&&(Object.keys(t).forEach(r=>{this.controls[r]&&this.controls[r].patchValue(t[r],{onlySelf:!0,emitEvent:n.emitEvent})}),this.updateValueAndValidity(n))}reset(t={},n={}){this._forEachChild((r,o)=>{r.reset(t[o],{onlySelf:!0,emitEvent:n.emitEvent})}),this._updatePristine(n),this._updateTouched(n),this.updateValueAndValidity(n)}getRawValue(){return this._reduceChildren({},(t,n,r)=>(t[r]=n instanceof La?n.value:n.getRawValue(),t))}_syncPendingControls(){let t=this._reduceChildren(!1,(n,r)=>!!r._syncPendingControls()||n);return t&&this.updateValueAndValidity({onlySelf:!0}),t}_throwIfControlMissing(t){if(!Object.keys(this.controls).length)throw new Error("\n        There are no form controls registered with this group yet. If you're using ngModel,\n        you may want to check next tick (e.g. use setTimeout).\n      ");if(!this.controls[t])throw new Error(`Cannot find form control with name: ${t}.`)}_forEachChild(t){Object.keys(this.controls).forEach(n=>{const r=this.controls[n];r&&t(r,n)})}_setUpControls(){this._forEachChild(t=>{t.setParent(this),t._registerOnCollectionChange(this._onCollectionChange)})}_updateValue(){this.value=this._reduceValue()}_anyControls(t){for(const n of Object.keys(this.controls)){const r=this.controls[n];if(this.contains(n)&&t(r))return!0}return!1}_reduceValue(){return this._reduceChildren({},(t,n,r)=>((n.enabled||this.disabled)&&(t[r]=n.value),t))}_reduceChildren(t,n){let r=t;return this._forEachChild((o,i)=>{r=n(r,o,i)}),r}_allControlsDisabled(){for(const t of Object.keys(this.controls))if(this.controls[t].enabled)return!1;return Object.keys(this.controls).length>0||this.disabled}_checkAllValuesPresent(t){this._forEachChild((n,r)=>{if(void 0===t[r])throw new Error(`Must supply a value for form control with name: '${r}'.`)})}}class RR extends wd{constructor(t,n,r){super(Dd(n),bd(r,n)),this.controls=t,this._initObservables(),this._setUpdateStrategy(n),this._setUpControls(),this.updateValueAndValidity({onlySelf:!0,emitEvent:!!this.asyncValidator})}at(t){return this.controls[t]}push(t,n={}){this.controls.push(t),this._registerControl(t),this.updateValueAndValidity({emitEvent:n.emitEvent}),this._onCollectionChange()}insert(t,n,r={}){this.controls.splice(t,0,n),this._registerControl(n),this.updateValueAndValidity({emitEvent:r.emitEvent})}removeAt(t,n={}){this.controls[t]&&this.controls[t]._registerOnCollectionChange(()=>{}),this.controls.splice(t,1),this.updateValueAndValidity({emitEvent:n.emitEvent})}setControl(t,n,r={}){this.controls[t]&&this.controls[t]._registerOnCollectionChange(()=>{}),this.controls.splice(t,1),n&&(this.controls.splice(t,0,n),this._registerControl(n)),this.updateValueAndValidity({emitEvent:r.emitEvent}),this._onCollectionChange()}get length(){return this.controls.length}setValue(t,n={}){this._checkAllValuesPresent(t),t.forEach((r,o)=>{this._throwIfControlMissing(o),this.at(o).setValue(r,{onlySelf:!0,emitEvent:n.emitEvent})}),this.updateValueAndValidity(n)}patchValue(t,n={}){null!=t&&(t.forEach((r,o)=>{this.at(o)&&this.at(o).patchValue(r,{onlySelf:!0,emitEvent:n.emitEvent})}),this.updateValueAndValidity(n))}reset(t=[],n={}){this._forEachChild((r,o)=>{r.reset(t[o],{onlySelf:!0,emitEvent:n.emitEvent})}),this._updatePristine(n),this._updateTouched(n),this.updateValueAndValidity(n)}getRawValue(){return this.controls.map(t=>t instanceof La?t.value:t.getRawValue())}clear(t={}){this.controls.length<1||(this._forEachChild(n=>n._registerOnCollectionChange(()=>{})),this.controls.splice(0),this.updateValueAndValidity({emitEvent:t.emitEvent}))}_syncPendingControls(){let t=this.controls.reduce((n,r)=>!!r._syncPendingControls()||n,!1);return t&&this.updateValueAndValidity({onlySelf:!0}),t}_throwIfControlMissing(t){if(!this.controls.length)throw new Error("\n        There are no form controls registered with this array yet. If you're using ngModel,\n        you may want to check next tick (e.g. use setTimeout).\n      ");if(!this.at(t))throw new Error(`Cannot find form control at index ${t}`)}_forEachChild(t){this.controls.forEach((n,r)=>{t(n,r)})}_updateValue(){this.value=this.controls.filter(t=>t.enabled||this.disabled).map(t=>t.value)}_anyControls(t){return this.controls.some(n=>n.enabled&&t(n))}_setUpControls(){this._forEachChild(t=>this._registerControl(t))}_checkAllValuesPresent(t){this._forEachChild((n,r)=>{if(void 0===t[r])throw new Error(`Must supply a value for form control at index: ${r}.`)})}_allControlsDisabled(){for(const t of this.controls)if(t.enabled)return!1;return this.controls.length>0||this.disabled}_registerControl(t){t.setParent(this),t._registerOnCollectionChange(this._onCollectionChange)}}const PR={provide:Wn,useExisting:ue(()=>Ba)},UC=(()=>Promise.resolve(null))();let Ba=(()=>{class e extends Wn{constructor(n,r,o,i){super(),this.control=new La,this._registered=!1,this.update=new ze,this._parent=n,this._setValidators(r),this._setAsyncValidators(o),this.valueAccessor=function(e,t){if(!t)return null;let n,r,o;return Array.isArray(t),t.forEach(i=>{i.constructor===Pi?n=i:function(e){return Object.getPrototypeOf(e.constructor)===mr}(i)?r=i:o=i}),o||r||n||null}(0,i)}ngOnChanges(n){this._checkForErrors(),this._registered||this._setUpControl(),"isDisabled"in n&&this._updateDisabled(n),function(e,t){if(!e.hasOwnProperty("model"))return!1;const n=e.model;return!!n.isFirstChange()||!Object.is(t,n.currentValue)}(n,this.viewModel)&&(this._updateValue(this.model),this.viewModel=this.model)}ngOnDestroy(){this.formDirective&&this.formDirective.removeControl(this)}get path(){return this._parent?function(e,t){return[...t.path,e]}(this.name,this._parent):[this.name]}get formDirective(){return this._parent?this._parent.formDirective:null}viewToModelUpdate(n){this.viewModel=n,this.update.emit(n)}_setUpControl(){this._setUpdateStrategy(),this._isStandalone()?this._setUpStandalone():this.formDirective.addControl(this),this._registered=!0}_setUpdateStrategy(){this.options&&null!=this.options.updateOn&&(this.control._updateOn=this.options.updateOn)}_isStandalone(){return!this._parent||!(!this.options||!this.options.standalone)}_setUpStandalone(){Vi(this.control,this),this.control.updateValueAndValidity({emitEvent:!1})}_checkForErrors(){this._isStandalone()||this._checkParentType(),this._checkName()}_checkParentType(){}_checkName(){this.options&&this.options.name&&(this.name=this.options.name),this._isStandalone()}_updateValue(n){UC.then(()=>{this.control.setValue(n,{emitViewToModelChange:!1})})}_updateDisabled(n){const r=n.isDisabled.currentValue,o=""===r||r&&"false"!==r;UC.then(()=>{o&&!this.control.disabled?this.control.disable():!o&&this.control.disabled&&this.control.enable()})}}return e.\u0275fac=function(n){return new(n||e)(I(rt,9),I(We,10),I(Gn,10),I(fn,10))},e.\u0275dir=L({type:e,selectors:[["","ngModel","",3,"formControlName","",3,"formControl",""]],inputs:{name:"name",isDisabled:["disabled","isDisabled"],model:["ngModel","model"],options:["ngModelOptions","options"]},outputs:{update:"ngModelChange"},exportAs:["ngModel"],features:[_e([PR]),ge,ft]}),e})(),zC=(()=>{class e{}return e.\u0275fac=function(n){return new(n||e)},e.\u0275mod=mn({type:e}),e.\u0275inj=Ft({}),e})();const HR={provide:fn,useExisting:ue(()=>Td),multi:!0};let Td=(()=>{class e extends mr{writeValue(n){this.setProperty("value",parseFloat(n))}registerOnChange(n){this.onChange=r=>{n(""==r?null:parseFloat(r))}}}return e.\u0275fac=function(){let t;return function(r){return(t||(t=Et(e)))(r||e)}}(),e.\u0275dir=L({type:e,selectors:[["input","type","range","formControlName",""],["input","type","range","formControl",""],["input","type","range","ngModel",""]],hostBindings:function(n,r){1&n&&Z("change",function(i){return r.onChange(i.target.value)})("input",function(i){return r.onChange(i.target.value)})("blur",function(){return r.onTouched()})},features:[_e([HR]),ge]}),e})();const WR={provide:fn,useExisting:ue(()=>Hi),multi:!0};function YC(e,t){return null==e?`${t}`:(t&&"object"==typeof t&&(t="Object"),`${e}: ${t}`.slice(0,50))}let Hi=(()=>{class e extends mr{constructor(){super(...arguments),this._optionMap=new Map,this._idCounter=0,this._compareWith=Object.is}set compareWith(n){this._compareWith=n}writeValue(n){this.value=n;const r=this._getOptionId(n);null==r&&this.setProperty("selectedIndex",-1);const o=YC(r,n);this.setProperty("value",o)}registerOnChange(n){this.onChange=r=>{this.value=this._getOptionValue(r),n(this.value)}}_registerOption(){return(this._idCounter++).toString()}_getOptionId(n){for(const r of Array.from(this._optionMap.keys()))if(this._compareWith(this._optionMap.get(r),n))return r;return null}_getOptionValue(n){const r=function(e){return e.split(":")[0]}(n);return this._optionMap.has(r)?this._optionMap.get(r):n}}return e.\u0275fac=function(){let t;return function(r){return(t||(t=Et(e)))(r||e)}}(),e.\u0275dir=L({type:e,selectors:[["select","formControlName","",3,"multiple",""],["select","formControl","",3,"multiple",""],["select","ngModel","",3,"multiple",""]],hostBindings:function(n,r){1&n&&Z("change",function(i){return r.onChange(i.target.value)})("blur",function(){return r.onTouched()})},inputs:{compareWith:"compareWith"},features:[_e([WR]),ge]}),e})(),Rd=(()=>{class e{constructor(n,r,o){this._element=n,this._renderer=r,this._select=o,this._select&&(this.id=this._select._registerOption())}set ngValue(n){null!=this._select&&(this._select._optionMap.set(this.id,n),this._setElementValue(YC(this.id,n)),this._select.writeValue(this._select.value))}set value(n){this._setElementValue(n),this._select&&this._select.writeValue(this._select.value)}_setElementValue(n){this._renderer.setProperty(this._element.nativeElement,"value",n)}ngOnDestroy(){this._select&&(this._select._optionMap.delete(this.id),this._select.writeValue(this._select.value))}}return e.\u0275fac=function(n){return new(n||e)(I($e),I(cr),I(Hi,9))},e.\u0275dir=L({type:e,selectors:[["option"]],inputs:{ngValue:"ngValue",value:"value"}}),e})();const QR={provide:fn,useExisting:ue(()=>Fd),multi:!0};function ZC(e,t){return null==e?`${t}`:("string"==typeof t&&(t=`'${t}'`),t&&"object"==typeof t&&(t="Object"),`${e}: ${t}`.slice(0,50))}let Fd=(()=>{class e extends mr{constructor(){super(...arguments),this._optionMap=new Map,this._idCounter=0,this._compareWith=Object.is}set compareWith(n){this._compareWith=n}writeValue(n){let r;if(this.value=n,Array.isArray(n)){const o=n.map(i=>this._getOptionId(i));r=(i,s)=>{i._setSelected(o.indexOf(s.toString())>-1)}}else r=(o,i)=>{o._setSelected(!1)};this._optionMap.forEach(r)}registerOnChange(n){this.onChange=r=>{const o=[];if(void 0!==r.selectedOptions){const i=r.selectedOptions;for(let s=0;s<i.length;s++){const a=i.item(s),l=this._getOptionValue(a.value);o.push(l)}}else{const i=r.options;for(let s=0;s<i.length;s++){const a=i.item(s);if(a.selected){const l=this._getOptionValue(a.value);o.push(l)}}}this.value=o,n(o)}}_registerOption(n){const r=(this._idCounter++).toString();return this._optionMap.set(r,n),r}_getOptionId(n){for(const r of Array.from(this._optionMap.keys()))if(this._compareWith(this._optionMap.get(r)._value,n))return r;return null}_getOptionValue(n){const r=function(e){return e.split(":")[0]}(n);return this._optionMap.has(r)?this._optionMap.get(r)._value:n}}return e.\u0275fac=function(){let t;return function(r){return(t||(t=Et(e)))(r||e)}}(),e.\u0275dir=L({type:e,selectors:[["select","multiple","","formControlName",""],["select","multiple","","formControl",""],["select","multiple","","ngModel",""]],hostBindings:function(n,r){1&n&&Z("change",function(i){return r.onChange(i.target)})("blur",function(){return r.onTouched()})},inputs:{compareWith:"compareWith"},features:[_e([QR]),ge]}),e})(),Od=(()=>{class e{constructor(n,r,o){this._element=n,this._renderer=r,this._select=o,this._select&&(this.id=this._select._registerOption(this))}set ngValue(n){null!=this._select&&(this._value=n,this._setElementValue(ZC(this.id,n)),this._select.writeValue(this._select.value))}set value(n){this._select?(this._value=n,this._setElementValue(ZC(this.id,n)),this._select.writeValue(this._select.value)):this._setElementValue(n)}_setElementValue(n){this._renderer.setProperty(this._element.nativeElement,"value",n)}_setSelected(n){this._renderer.setProperty(this._element.nativeElement,"selected",n)}ngOnDestroy(){this._select&&(this._select._optionMap.delete(this.id),this._select.writeValue(this._select.value))}}return e.\u0275fac=function(n){return new(n||e)(I($e),I(cr),I(Fd,9))},e.\u0275dir=L({type:e,selectors:[["option"]],inputs:{ngValue:"ngValue",value:"value"}}),e})(),av=(()=>{class e{}return e.\u0275fac=function(n){return new(n||e)},e.\u0275mod=mn({type:e}),e.\u0275inj=Ft({imports:[[zC]]}),e})(),oF=(()=>{class e{}return e.\u0275fac=function(n){return new(n||e)},e.\u0275mod=mn({type:e}),e.\u0275inj=Ft({imports:[av]}),e})();class lv{constructor(){this.riskHotspotsSettings=null,this.coverageInfoSettings=null}}class iF{constructor(){this.groupingMaximum=0,this.grouping=0,this.historyComparisionDate="",this.historyComparisionType="",this.filter="",this.sortBy="name",this.sortOrder="asc",this.collapseStates=[]}}class sF{constructor(t){this.et="",this.et=t.et,this.cl=t.cl,this.ucl=t.ucl,this.cal=t.cal,this.tl=t.tl,this.lcq=t.lcq,this.cb=t.cb,this.tb=t.tb,this.bcq=t.bcq}get coverageRatioText(){return 0===this.tl?"-":this.cl+"/"+this.cal}get branchCoverageRatioText(){return 0===this.tb?"-":this.cb+"/"+this.tb}}class vo{static roundNumber(t,n){return Math.floor(t*Math.pow(10,n))/Math.pow(10,n)}static getNthOrLastIndexOf(t,n,r){let o=0,i=-1,s=-1;for(;o<r&&(s=t.indexOf(n,i+1),-1!==s);)i=s,o++;return i}}class cv{constructor(){this.name="",this.coveredLines=0,this.uncoveredLines=0,this.coverableLines=0,this.totalLines=0,this.coveredBranches=0,this.totalBranches=0}get coverage(){return 0===this.coverableLines?NaN:vo.roundNumber(100*this.coveredLines/this.coverableLines,1)}get coveragePercentage(){return 0===this.coverableLines?"":this.coverage+"%"}get coverageRatioText(){return 0===this.coverableLines?"-":this.coveredLines+"/"+this.coverableLines}get branchCoverage(){return 0===this.totalBranches?NaN:vo.roundNumber(100*this.coveredBranches/this.totalBranches,1)}get branchCoveragePercentage(){return 0===this.totalBranches?"":this.branchCoverage+"%"}get branchCoverageRatioText(){return 0===this.totalBranches?"-":this.coveredBranches+"/"+this.totalBranches}}class Vd extends cv{constructor(t,n){super(),this.reportPath="",this._coverageType="",this.methodCoverage="",this.lineCoverageHistory=[],this.branchCoverageHistory=[],this.historicCoverages=[],this.currentHistoricCoverage=null,this.name=t.name,this.reportPath=t.rp?t.rp+n:t.rp,this.coveredLines=t.cl,this.uncoveredLines=t.ucl,this.coverableLines=t.cal,this.totalLines=t.tl,this._coverageType=t.ct,this.methodCoverage=t.mc,this.coveredBranches=t.cb,this.totalBranches=t.tb,this.lineCoverageHistory=t.lch,this.branchCoverageHistory=t.bch,t.hc.forEach(r=>{this.historicCoverages.push(new sF(r))})}get coverage(){return 0===this.coverableLines?"-"!==this.methodCoverage?parseFloat(this.methodCoverage):NaN:vo.roundNumber(100*this.coveredLines/this.coverableLines,1)}get coverageType(){return 0===this.coverableLines?"-"!==this.methodCoverage?this._coverageType:"":this._coverageType}visible(t,n){if(""!==t&&-1===this.name.toLowerCase().indexOf(t.toLowerCase()))return!1;if(""===n||null===this.currentHistoricCoverage)return!0;if("allChanges"===n){if(this.coveredLines===this.currentHistoricCoverage.cl&&this.uncoveredLines===this.currentHistoricCoverage.ucl&&this.coverableLines===this.currentHistoricCoverage.cal&&this.totalLines===this.currentHistoricCoverage.tl&&this.coveredBranches===this.currentHistoricCoverage.cb&&this.totalBranches===this.currentHistoricCoverage.tb)return!1}else if("lineCoverageIncreaseOnly"===n){let r=this.coverage;if(isNaN(r)||r<=this.currentHistoricCoverage.lcq)return!1}else if("lineCoverageDecreaseOnly"===n){let r=this.coverage;if(isNaN(r)||r>=this.currentHistoricCoverage.lcq)return!1}else if("branchCoverageIncreaseOnly"===n){let r=this.branchCoverage;if(isNaN(r)||r<=this.currentHistoricCoverage.bcq)return!1}else if("branchCoverageDecreaseOnly"===n){let r=this.branchCoverage;if(isNaN(r)||r>=this.currentHistoricCoverage.bcq)return!1}return!0}updateCurrentHistoricCoverage(t){if(this.currentHistoricCoverage=null,""!==t)for(let n=0;n<this.historicCoverages.length;n++)if(this.historicCoverages[n].et===t){this.currentHistoricCoverage=this.historicCoverages[n];break}}}class qn extends cv{constructor(t,n){super(),this.subElements=[],this.classes=[],this.collapsed=!1,this.name=t,this.collapsed=t.indexOf("Test")>-1&&null===n}visible(t,n){if(""!==t&&this.name.toLowerCase().indexOf(t.toLowerCase())>-1)return!0;for(let r=0;r<this.subElements.length;r++)if(this.subElements[r].visible(t,n))return!0;for(let r=0;r<this.classes.length;r++)if(this.classes[r].visible(t,n))return!0;return!1}insertClass(t,n){if(this.coveredLines+=t.coveredLines,this.uncoveredLines+=t.uncoveredLines,this.coverableLines+=t.coverableLines,this.totalLines+=t.totalLines,this.coveredBranches+=t.coveredBranches,this.totalBranches+=t.totalBranches,null===n)return void this.classes.push(t);let r=vo.getNthOrLastIndexOf(t.name,".",n),o=-1===r?"-":t.name.substr(0,r);for(let s=0;s<this.subElements.length;s++)if(this.subElements[s].name===o)return void this.subElements[s].insertClass(t,null);let i=new qn(o,this);this.subElements.push(i),i.insertClass(t,null)}collapse(){this.collapsed=!0;for(let t=0;t<this.subElements.length;t++)this.subElements[t].collapse()}expand(){this.collapsed=!1;for(let t=0;t<this.subElements.length;t++)this.subElements[t].expand()}toggleCollapse(t){t.preventDefault(),this.collapsed=!this.collapsed}updateCurrentHistoricCoverage(t){for(let n=0;n<this.subElements.length;n++)this.subElements[n].updateCurrentHistoricCoverage(t);for(let n=0;n<this.classes.length;n++)this.classes[n].updateCurrentHistoricCoverage(t)}static sortCodeElementViewModels(t,n,r){let o=r?-1:1,i=r?1:-1;"name"===n?t.sort(function(s,a){return s.name===a.name?0:s.name<a.name?o:i}):"covered"===n?t.sort(function(s,a){return s.coveredLines===a.coveredLines?0:s.coveredLines<a.coveredLines?o:i}):"uncovered"===n?t.sort(function(s,a){return s.uncoveredLines===a.uncoveredLines?0:s.uncoveredLines<a.uncoveredLines?o:i}):"coverable"===n?t.sort(function(s,a){return s.coverableLines===a.coverableLines?0:s.coverableLines<a.coverableLines?o:i}):"total"===n?t.sort(function(s,a){return s.totalLines===a.totalLines?0:s.totalLines<a.totalLines?o:i}):"coverage"===n?t.sort(function(s,a){return s.coverage===a.coverage?0:isNaN(s.coverage)?o:isNaN(a.coverage)?i:s.coverage<a.coverage?o:i}):"branchcoverage"===n&&t.sort(function(s,a){return s.branchCoverage===a.branchCoverage?0:isNaN(s.branchCoverage)?o:isNaN(a.branchCoverage)?i:s.branchCoverage<a.branchCoverage?o:i})}changeSorting(t,n){qn.sortCodeElementViewModels(this.subElements,t,n);let r=n?-1:1,o=n?1:-1;"name"===t?this.classes.sort(function(i,s){return i.name===s.name?0:i.name<s.name?r:o}):"covered"===t?this.classes.sort(function(i,s){return i.coveredLines===s.coveredLines?0:i.coveredLines<s.coveredLines?r:o}):"uncovered"===t?this.classes.sort(function(i,s){return i.uncoveredLines===s.uncoveredLines?0:i.uncoveredLines<s.uncoveredLines?r:o}):"coverable"===t?this.classes.sort(function(i,s){return i.coverableLines===s.coverableLines?0:i.coverableLines<s.coverableLines?r:o}):"total"===t?this.classes.sort(function(i,s){return i.totalLines===s.totalLines?0:i.totalLines<s.totalLines?r:o}):"coverage"===t?this.classes.sort(function(i,s){return i.coverage===s.coverage?0:isNaN(i.coverage)?r:isNaN(s.coverage)?o:i.coverage<s.coverage?r:o}):"covered_branches"===t?this.classes.sort(function(i,s){return i.coveredBranches===s.coveredBranches?0:i.coveredBranches<s.coveredBranches?r:o}):"total_branches"===t?this.classes.sort(function(i,s){return i.totalBranches===s.totalBranches?0:i.totalBranches<s.totalBranches?r:o}):"branchcoverage"===t&&this.classes.sort(function(i,s){return i.branchCoverage===s.branchCoverage?0:isNaN(i.branchCoverage)?r:isNaN(s.branchCoverage)?o:i.branchCoverage<s.branchCoverage?r:o});for(let i=0;i<this.subElements.length;i++)this.subElements[i].changeSorting(t,n)}}let kd=(()=>{class e{get nativeWindow(){return window}}return e.\u0275fac=function(n){return new(n||e)},e.\u0275prov=te({token:e,factory:e.\u0275fac}),e})();function lF(e,t){1&e&&k(0,"td",3)}function cF(e,t){1&e&&k(0,"td"),2&e&&Ln("green ",w().greenClass,"")}function uF(e,t){1&e&&k(0,"td"),2&e&&Ln("red ",w().redClass,"")}let uv=(()=>{class e{constructor(){this.grayVisible=!0,this.greenVisible=!1,this.redVisible=!1,this.greenClass="",this.redClass="",this._percentage=NaN}get percentage(){return this._percentage}set percentage(n){this._percentage=n,this.grayVisible=isNaN(n),this.greenVisible=!isNaN(n)&&Math.round(n)>0,this.redVisible=!isNaN(n)&&100-Math.round(n)>0,this.greenClass="covered"+Math.round(n),this.redClass="covered"+(100-Math.round(n))}}return e.\u0275fac=function(n){return new(n||e)},e.\u0275cmp=xn({type:e,selectors:[["coverage-bar"]],inputs:{percentage:"percentage"},decls:4,vars:3,consts:[[1,"coverage"],["class","gray covered100",4,"ngIf"],[3,"class",4,"ngIf"],[1,"gray","covered100"]],template:function(n,r){1&n&&(y(0,"table",0),S(1,lF,1,0,"td",1),S(2,cF,1,3,"td",2),S(3,uF,1,3,"td",2),C()),2&n&&(g(1),D("ngIf",r.grayVisible),g(1),D("ngIf",r.greenVisible),g(1),D("ngIf",r.redVisible))},directives:[yo],encapsulation:2,changeDetection:0}),e})();const dF=["codeelement-row",""];function fF(e,t){if(1&e&&(y(0,"th",2),M(1),C()),2&e){const n=w();g(1),P(n.element.coveredBranches)}}function hF(e,t){if(1&e&&(y(0,"th",2),M(1),C()),2&e){const n=w();g(1),P(n.element.totalBranches)}}function pF(e,t){if(1&e&&(y(0,"th",3),M(1),C()),2&e){const n=w();D("title",n.element.branchCoverageRatioText),g(1),P(n.element.branchCoveragePercentage)}}function gF(e,t){if(1&e&&(y(0,"th",2),k(1,"coverage-bar",4),C()),2&e){const n=w();g(1),D("percentage",n.element.branchCoverage)}}const mF=function(e,t){return{"icon-plus":e,"icon-minus":t}};let _F=(()=>{class e{constructor(){this.collapsed=!1,this.branchCoverageAvailable=!1}}return e.\u0275fac=function(n){return new(n||e)},e.\u0275cmp=xn({type:e,selectors:[["","codeelement-row",""]],inputs:{element:"element",collapsed:"collapsed",branchCoverageAvailable:"branchCoverageAvailable"},attrs:dF,decls:20,vars:16,consts:[["href","#",3,"click"],[3,"ngClass"],[1,"right"],[1,"right",3,"title"],[3,"percentage"],["class","right",4,"ngIf"],["class","right",3,"title",4,"ngIf"]],template:function(n,r){1&n&&(y(0,"th"),y(1,"a",0),Z("click",function(i){return r.element.toggleCollapse(i)}),k(2,"i",1),M(3),C(),C(),y(4,"th",2),M(5),C(),y(6,"th",2),M(7),C(),y(8,"th",2),M(9),C(),y(10,"th",2),M(11),C(),y(12,"th",3),M(13),C(),y(14,"th",2),k(15,"coverage-bar",4),C(),S(16,fF,2,1,"th",5),S(17,hF,2,1,"th",5),S(18,pF,2,2,"th",6),S(19,gF,2,1,"th",5)),2&n&&(g(2),D("ngClass",su(13,mF,r.element.collapsed,!r.element.collapsed)),g(1),oe(" ",r.element.name,""),g(2),P(r.element.coveredLines),g(2),P(r.element.uncoveredLines),g(2),P(r.element.coverableLines),g(2),P(r.element.totalLines),g(1),D("title",r.element.coverageRatioText),g(1),P(r.element.coveragePercentage),g(2),D("percentage",r.element.coverage),g(1),D("ngIf",r.branchCoverageAvailable),g(1),D("ngIf",r.branchCoverageAvailable),g(1),D("ngIf",r.branchCoverageAvailable),g(1),D("ngIf",r.branchCoverageAvailable))},directives:[Ni,uv,yo],encapsulation:2,changeDetection:0}),e})();const yF=["coverage-history-chart",""];let CF=(()=>{class e{constructor(){this.path=null,this._historicCoverages=[]}get historicCoverages(){return this._historicCoverages}set historicCoverages(n){if(this._historicCoverages=n,n.length>1){let r="";for(let o=0;o<n.length;o++)r+=0===o?"M":"L",r+=`${vo.roundNumber(30*o/(n.length-1),1)}`,r+=`,${vo.roundNumber(18-18*n[o]/100,1)}`;this.path=r}else this.path=null}}return e.\u0275fac=function(n){return new(n||e)},e.\u0275cmp=xn({type:e,selectors:[["","coverage-history-chart",""]],inputs:{historicCoverages:"historicCoverages"},attrs:yF,decls:3,vars:1,consts:[["width","30","height","18",1,"ct-chart-line"],[1,"ct-series","ct-series-a"],[1,"ct-line"]],template:function(n,r){1&n&&(B.lFrame.currentNamespace=Cf,y(0,"svg",0),y(1,"g",1),k(2,"path",2),C(),C()),2&n&&(g(2),an("d",r.path))},encapsulation:2,changeDetection:0}),e})();const vF=["class-row",""];function DF(e,t){if(1&e&&(y(0,"a",8),M(1),C()),2&e){const n=w();D("href",n.clazz.reportPath,Vr),g(1),P(n.clazz.name)}}function bF(e,t){if(1&e&&(se(0),M(1),ae()),2&e){const n=w();g(1),P(n.clazz.name)}}function EF(e,t){if(1&e&&(se(0),y(1,"div"),M(2),C(),y(3,"div",9),M(4),C(),ae()),2&e){const n=w();g(1),Ln("currenthistory ",n.getClassName(n.clazz.coveredLines,n.clazz.currentHistoricCoverage.cl),""),g(1),oe(" ",n.clazz.coveredLines," "),g(1),D("title",n.clazz.currentHistoricCoverage.et),g(1),oe(" ",n.clazz.currentHistoricCoverage.cl," ")}}function wF(e,t){if(1&e&&(se(0),M(1),ae()),2&e){const n=w();g(1),oe(" ",n.clazz.coveredLines," ")}}function IF(e,t){if(1&e&&(se(0),y(1,"div"),M(2),C(),y(3,"div",9),M(4),C(),ae()),2&e){const n=w();g(1),Ln("currenthistory ",n.getClassName(n.clazz.currentHistoricCoverage.ucl,n.clazz.uncoveredLines),""),g(1),oe(" ",n.clazz.uncoveredLines," "),g(1),D("title",n.clazz.currentHistoricCoverage.et),g(1),oe(" ",n.clazz.currentHistoricCoverage.ucl," ")}}function MF(e,t){if(1&e&&(se(0),M(1),ae()),2&e){const n=w();g(1),oe(" ",n.clazz.uncoveredLines," ")}}function TF(e,t){if(1&e&&(se(0),y(1,"div",10),M(2),C(),y(3,"div",9),M(4),C(),ae()),2&e){const n=w();g(2),P(n.clazz.coverableLines),g(1),D("title",n.clazz.currentHistoricCoverage.et),g(1),P(n.clazz.currentHistoricCoverage.cal)}}function AF(e,t){if(1&e&&(se(0),M(1),ae()),2&e){const n=w();g(1),oe(" ",n.clazz.coverableLines," ")}}function SF(e,t){if(1&e&&(se(0),y(1,"div",10),M(2),C(),y(3,"div",9),M(4),C(),ae()),2&e){const n=w();g(2),P(n.clazz.totalLines),g(1),D("title",n.clazz.currentHistoricCoverage.et),g(1),P(n.clazz.currentHistoricCoverage.tl)}}function xF(e,t){if(1&e&&(se(0),M(1),ae()),2&e){const n=w();g(1),oe(" ",n.clazz.totalLines," ")}}const dv=function(e){return{historiccoverageoffset:e}};function NF(e,t){if(1&e&&k(0,"div",11),2&e){const n=w();oi("title",n.translations.history+": "+n.translations.coverage),D("historicCoverages",n.clazz.lineCoverageHistory)("ngClass",iu(3,dv,null!==n.clazz.currentHistoricCoverage))}}function RF(e,t){if(1&e&&(se(0),y(1,"div"),M(2),C(),y(3,"div",9),M(4),C(),ae()),2&e){const n=w();g(1),Ln("currenthistory ",n.getClassName(n.clazz.coverage,n.clazz.currentHistoricCoverage.lcq),""),g(1),oe(" ",n.clazz.coveragePercentage," "),g(1),D("title",n.clazz.currentHistoricCoverage.et+": "+n.clazz.currentHistoricCoverage.coverageRatioText),g(1),oe("",n.clazz.currentHistoricCoverage.lcq,"%")}}function FF(e,t){if(1&e&&(se(0),M(1),ae()),2&e){const n=w();g(1),oe(" ",n.clazz.coveragePercentage," ")}}function OF(e,t){if(1&e&&(se(0),y(1,"div"),M(2),C(),y(3,"div",9),M(4),C(),ae()),2&e){const n=w(2);g(1),Ln("currenthistory ",n.getClassName(n.clazz.coveredBranches,n.clazz.currentHistoricCoverage.cb),""),g(1),oe(" ",n.clazz.coveredBranches," "),g(1),D("title",n.clazz.currentHistoricCoverage.et),g(1),oe(" ",n.clazz.currentHistoricCoverage.cb," ")}}function PF(e,t){if(1&e&&(se(0),M(1),ae()),2&e){const n=w(2);g(1),oe(" ",n.clazz.coveredBranches," ")}}function VF(e,t){if(1&e&&(y(0,"td",2),S(1,OF,5,6,"ng-container",1),S(2,PF,2,1,"ng-container",1),C()),2&e){const n=w();g(1),D("ngIf",null!==n.clazz.currentHistoricCoverage),g(1),D("ngIf",null===n.clazz.currentHistoricCoverage)}}function kF(e,t){if(1&e&&(se(0),y(1,"div",10),M(2),C(),y(3,"div",9),M(4),C(),ae()),2&e){const n=w(2);g(2),P(n.clazz.totalBranches),g(1),D("title",n.clazz.currentHistoricCoverage.et),g(1),P(n.clazz.currentHistoricCoverage.tb)}}function LF(e,t){if(1&e&&(se(0),M(1),ae()),2&e){const n=w(2);g(1),oe(" ",n.clazz.totalBranches," ")}}function BF(e,t){if(1&e&&(y(0,"td",2),S(1,kF,5,3,"ng-container",1),S(2,LF,2,1,"ng-container",1),C()),2&e){const n=w();g(1),D("ngIf",null!==n.clazz.currentHistoricCoverage),g(1),D("ngIf",null===n.clazz.currentHistoricCoverage)}}function HF(e,t){if(1&e&&k(0,"div",13),2&e){const n=w(2);oi("title",n.translations.history+": "+n.translations.branchCoverage),D("historicCoverages",n.clazz.branchCoverageHistory)("ngClass",iu(3,dv,null!==n.clazz.currentHistoricCoverage))}}function jF(e,t){if(1&e&&(se(0),y(1,"div"),M(2),C(),y(3,"div",9),M(4),C(),ae()),2&e){const n=w(2);g(1),Ln("currenthistory ",n.getClassName(n.clazz.branchCoverage,n.clazz.currentHistoricCoverage.bcq),""),g(1),oe(" ",n.clazz.branchCoveragePercentage," "),g(1),D("title",n.clazz.currentHistoricCoverage.et+": "+n.clazz.currentHistoricCoverage.branchCoverageRatioText),g(1),oe("",n.clazz.currentHistoricCoverage.bcq,"%")}}function $F(e,t){if(1&e&&(se(0),M(1),ae()),2&e){const n=w(2);g(1),oe(" ",n.clazz.branchCoveragePercentage," ")}}function UF(e,t){if(1&e&&(y(0,"td",3),S(1,HF,1,5,"div",12),S(2,jF,5,6,"ng-container",1),S(3,$F,2,1,"ng-container",1),C()),2&e){const n=w();D("title",n.clazz.branchCoverageRatioText),g(1),D("ngIf",n.clazz.branchCoverageHistory.length>1),g(1),D("ngIf",null!==n.clazz.currentHistoricCoverage),g(1),D("ngIf",null===n.clazz.currentHistoricCoverage)}}function GF(e,t){if(1&e&&(y(0,"td",2),k(1,"coverage-bar",5),C()),2&e){const n=w();g(1),D("percentage",n.clazz.branchCoverage)}}let zF=(()=>{class e{constructor(){this.translations={},this.branchCoverageAvailable=!1,this.historyComparisionDate=""}getClassName(n,r){return n>r?"lightgreen":n<r?"lightred":"lightgraybg"}}return e.\u0275fac=function(n){return new(n||e)},e.\u0275cmp=xn({type:e,selectors:[["","class-row",""]],inputs:{clazz:"clazz",translations:"translations",branchCoverageAvailable:"branchCoverageAvailable",historyComparisionDate:"historyComparisionDate"},attrs:vF,decls:25,vars:19,consts:[[3,"href",4,"ngIf"],[4,"ngIf"],[1,"right"],[1,"right",3,"title"],["coverage-history-chart","","class","tinylinecoveragechart ct-chart",3,"historicCoverages","ngClass","title",4,"ngIf"],[3,"percentage"],["class","right",4,"ngIf"],["class","right",3,"title",4,"ngIf"],[3,"href"],[3,"title"],[1,"currenthistory"],["coverage-history-chart","",1,"tinylinecoveragechart","ct-chart",3,"historicCoverages","ngClass","title"],["coverage-history-chart","","class","tinybranchcoveragechart ct-chart",3,"historicCoverages","ngClass","title",4,"ngIf"],["coverage-history-chart","",1,"tinybranchcoveragechart","ct-chart",3,"historicCoverages","ngClass","title"]],template:function(n,r){1&n&&(y(0,"td"),S(1,DF,2,2,"a",0),S(2,bF,2,1,"ng-container",1),C(),y(3,"td",2),S(4,EF,5,6,"ng-container",1),S(5,wF,2,1,"ng-container",1),C(),y(6,"td",2),S(7,IF,5,6,"ng-container",1),S(8,MF,2,1,"ng-container",1),C(),y(9,"td",2),S(10,TF,5,3,"ng-container",1),S(11,AF,2,1,"ng-container",1),C(),y(12,"td",2),S(13,SF,5,3,"ng-container",1),S(14,xF,2,1,"ng-container",1),C(),y(15,"td",3),S(16,NF,1,5,"div",4),S(17,RF,5,6,"ng-container",1),S(18,FF,2,1,"ng-container",1),C(),y(19,"td",2),k(20,"coverage-bar",5),C(),S(21,VF,3,2,"td",6),S(22,BF,3,2,"td",6),S(23,UF,4,4,"td",7),S(24,GF,2,1,"td",6)),2&n&&(g(1),D("ngIf",""!==r.clazz.reportPath),g(1),D("ngIf",""===r.clazz.reportPath),g(2),D("ngIf",null!==r.clazz.currentHistoricCoverage),g(1),D("ngIf",null===r.clazz.currentHistoricCoverage),g(2),D("ngIf",null!==r.clazz.currentHistoricCoverage),g(1),D("ngIf",null===r.clazz.currentHistoricCoverage),g(2),D("ngIf",null!==r.clazz.currentHistoricCoverage),g(1),D("ngIf",null===r.clazz.currentHistoricCoverage),g(2),D("ngIf",null!==r.clazz.currentHistoricCoverage),g(1),D("ngIf",null===r.clazz.currentHistoricCoverage),g(1),D("title",r.clazz.coverageType+": "+r.clazz.coverageRatioText),g(1),D("ngIf",r.clazz.lineCoverageHistory.length>1),g(1),D("ngIf",null!==r.clazz.currentHistoricCoverage),g(1),D("ngIf",null===r.clazz.currentHistoricCoverage),g(2),D("percentage",r.clazz.coverage),g(1),D("ngIf",r.branchCoverageAvailable),g(1),D("ngIf",r.branchCoverageAvailable),g(1),D("ngIf",r.branchCoverageAvailable),g(1),D("ngIf",r.branchCoverageAvailable))},directives:[yo,uv,CF,Ni],encapsulation:2,changeDetection:0}),e})();function WF(e,t){if(1&e&&(se(0),M(1),ae()),2&e){const n=w(2);g(1),P(n.translations.noGrouping)}}function qF(e,t){if(1&e&&(se(0),M(1),ae()),2&e){const n=w(2);g(1),P(n.translations.byAssembly)}}function QF(e,t){if(1&e&&(se(0),M(1),ae()),2&e){const n=w(2);g(1),P(n.translations.byNamespace+" "+n.settings.grouping)}}function KF(e,t){if(1&e&&(y(0,"option",26),M(1),C()),2&e){const n=t.$implicit;D("value",n),g(1),P(n)}}function YF(e,t){1&e&&k(0,"br")}function ZF(e,t){if(1&e&&(y(0,"option",32),M(1),C()),2&e){const n=w(4);g(1),oe(" ",n.translations.branchCoverageIncreaseOnly," ")}}function JF(e,t){if(1&e&&(y(0,"option",33),M(1),C()),2&e){const n=w(4);g(1),oe(" ",n.translations.branchCoverageDecreaseOnly," ")}}function XF(e,t){if(1&e){const n=ln();y(0,"div"),y(1,"select",23),Z("ngModelChange",function(o){return le(n),w(3).settings.historyComparisionType=o}),y(2,"option",24),M(3),C(),y(4,"option",27),M(5),C(),y(6,"option",28),M(7),C(),y(8,"option",29),M(9),C(),S(10,ZF,2,1,"option",30),S(11,JF,2,1,"option",31),C(),C()}if(2&e){const n=w(3);g(1),D("ngModel",n.settings.historyComparisionType),g(2),P(n.translations.filter),g(2),P(n.translations.allChanges),g(2),P(n.translations.lineCoverageIncreaseOnly),g(2),P(n.translations.lineCoverageDecreaseOnly),g(1),D("ngIf",n.branchCoverageAvailable),g(1),D("ngIf",n.branchCoverageAvailable)}}function eO(e,t){if(1&e){const n=ln();se(0),y(1,"div"),M(2),y(3,"select",23),Z("ngModelChange",function(o){return le(n),w(2).settings.historyComparisionDate=o})("ngModelChange",function(){return le(n),w(2).updateCurrentHistoricCoverage()}),y(4,"option",24),M(5),C(),S(6,KF,2,2,"option",25),C(),C(),S(7,YF,1,0,"br",0),S(8,XF,12,7,"div",0),ae()}if(2&e){const n=w(2);g(2),oe(" ",n.translations.compareHistory," "),g(1),D("ngModel",n.settings.historyComparisionDate),g(2),P(n.translations.date),g(1),D("ngForOf",n.historicCoverageExecutionTimes),g(1),D("ngIf",""!==n.settings.historyComparisionDate),g(1),D("ngIf",""!==n.settings.historyComparisionDate)}}function tO(e,t){1&e&&k(0,"col",8)}function nO(e,t){1&e&&k(0,"col",11)}function rO(e,t){1&e&&k(0,"col",12)}function oO(e,t){1&e&&k(0,"col",13)}const In=function(e,t,n){return{"icon-up-dir_active":e,"icon-down-dir_active":t,"icon-down-dir":n}};function iO(e,t){if(1&e){const n=ln();y(0,"th",5),y(1,"a",2),Z("click",function(o){return le(n),w(2).updateSorting("covered_branches",o)}),k(2,"i",18),M(3),C(),C()}if(2&e){const n=w(2);g(2),D("ngClass",st(2,In,"covered_branches"===n.settings.sortBy&&"desc"===n.settings.sortOrder,"covered_branches"===n.settings.sortBy&&"asc"===n.settings.sortOrder,"covered_branches"!==n.settings.sortBy)),g(1),P(n.translations.covered)}}function sO(e,t){if(1&e){const n=ln();y(0,"th",5),y(1,"a",2),Z("click",function(o){return le(n),w(2).updateSorting("total_branches",o)}),k(2,"i",18),M(3),C(),C()}if(2&e){const n=w(2);g(2),D("ngClass",st(2,In,"total_branches"===n.settings.sortBy&&"desc"===n.settings.sortOrder,"total_branches"===n.settings.sortBy&&"asc"===n.settings.sortOrder,"total_branches"!==n.settings.sortBy)),g(1),P(n.translations.total)}}function aO(e,t){if(1&e){const n=ln();y(0,"th",19),y(1,"a",2),Z("click",function(o){return le(n),w(2).updateSorting("branchcoverage",o)}),k(2,"i",18),M(3),C(),C()}if(2&e){const n=w(2);g(2),D("ngClass",st(2,In,"branchcoverage"===n.settings.sortBy&&"desc"===n.settings.sortOrder,"branchcoverage"===n.settings.sortBy&&"asc"===n.settings.sortOrder,"branchcoverage"!==n.settings.sortBy)),g(1),P(n.translations.branchCoverage)}}function lO(e,t){if(1&e&&k(0,"tr",35),2&e){const n=w().$implicit,r=w(2);D("element",n)("collapsed",n.collapsed)("branchCoverageAvailable",r.branchCoverageAvailable)}}function cO(e,t){if(1&e&&k(0,"tr",37),2&e){const n=w().$implicit,r=w(3);D("clazz",n)("translations",r.translations)("branchCoverageAvailable",r.branchCoverageAvailable)("historyComparisionDate",r.settings.historyComparisionDate)}}function uO(e,t){if(1&e&&(se(0),S(1,cO,1,4,"tr",36),ae()),2&e){const n=t.$implicit,r=w().$implicit,o=w(2);g(1),D("ngIf",!r.collapsed&&n.visible(o.settings.filter,o.settings.historyComparisionType))}}function dO(e,t){if(1&e&&k(0,"tr",40),2&e){const n=w().$implicit,r=w(5);D("clazz",n)("translations",r.translations)("branchCoverageAvailable",r.branchCoverageAvailable)("historyComparisionDate",r.settings.historyComparisionDate)}}function fO(e,t){if(1&e&&(se(0),S(1,dO,1,4,"tr",39),ae()),2&e){const n=t.$implicit,r=w(2).$implicit,o=w(3);g(1),D("ngIf",!r.collapsed&&n.visible(o.settings.filter,o.settings.historyComparisionType))}}function hO(e,t){if(1&e&&(se(0),k(1,"tr",38),S(2,fO,2,1,"ng-container",22),ae()),2&e){const n=w().$implicit,r=w(3);g(1),D("element",n)("collapsed",n.collapsed)("branchCoverageAvailable",r.branchCoverageAvailable),g(1),D("ngForOf",n.classes)}}function pO(e,t){if(1&e&&(se(0),S(1,hO,3,4,"ng-container",0),ae()),2&e){const n=t.$implicit,r=w().$implicit,o=w(2);g(1),D("ngIf",!r.collapsed&&n.visible(o.settings.filter,o.settings.historyComparisionType))}}function gO(e,t){if(1&e&&(se(0),S(1,lO,1,3,"tr",34),S(2,uO,2,1,"ng-container",22),S(3,pO,2,1,"ng-container",22),ae()),2&e){const n=t.$implicit,r=w(2);g(1),D("ngIf",n.visible(r.settings.filter,r.settings.historyComparisionType)),g(1),D("ngForOf",n.classes),g(1),D("ngForOf",n.subElements)}}function mO(e,t){if(1&e){const n=ln();y(0,"div"),y(1,"div",1),y(2,"div"),y(3,"a",2),Z("click",function(o){return le(n),w().collapseAll(o)}),M(4),C(),M(5," | "),y(6,"a",2),Z("click",function(o){return le(n),w().expandAll(o)}),M(7),C(),C(),y(8,"div",3),S(9,WF,2,1,"ng-container",0),S(10,qF,2,1,"ng-container",0),S(11,QF,2,1,"ng-container",0),k(12,"br"),M(13),y(14,"input",4),Z("ngModelChange",function(o){return le(n),w().settings.grouping=o})("ngModelChange",function(){return le(n),w().updateCoverageInfo()}),C(),C(),y(15,"div",3),S(16,eO,9,6,"ng-container",0),C(),y(17,"div",5),y(18,"span"),M(19),C(),y(20,"input",6),Z("ngModelChange",function(o){return le(n),w().settings.filter=o}),C(),C(),C(),y(21,"table",7),y(22,"colgroup"),k(23,"col"),k(24,"col",8),k(25,"col",9),k(26,"col",10),k(27,"col",11),k(28,"col",12),k(29,"col",13),S(30,tO,1,0,"col",14),S(31,nO,1,0,"col",15),S(32,rO,1,0,"col",16),S(33,oO,1,0,"col",17),C(),y(34,"thead"),y(35,"tr"),y(36,"th"),y(37,"a",2),Z("click",function(o){return le(n),w().updateSorting("name",o)}),k(38,"i",18),M(39),C(),C(),y(40,"th",5),y(41,"a",2),Z("click",function(o){return le(n),w().updateSorting("covered",o)}),k(42,"i",18),M(43),C(),C(),y(44,"th",5),y(45,"a",2),Z("click",function(o){return le(n),w().updateSorting("uncovered",o)}),k(46,"i",18),M(47),C(),C(),y(48,"th",5),y(49,"a",2),Z("click",function(o){return le(n),w().updateSorting("coverable",o)}),k(50,"i",18),M(51),C(),C(),y(52,"th",5),y(53,"a",2),Z("click",function(o){return le(n),w().updateSorting("total",o)}),k(54,"i",18),M(55),C(),C(),y(56,"th",19),y(57,"a",2),Z("click",function(o){return le(n),w().updateSorting("coverage",o)}),k(58,"i",18),M(59),C(),C(),S(60,iO,4,6,"th",20),S(61,sO,4,6,"th",20),S(62,aO,4,6,"th",21),C(),C(),y(63,"tbody"),S(64,gO,4,3,"ng-container",22),C(),C(),C()}if(2&e){const n=w();g(4),P(n.translations.collapseAll),g(3),P(n.translations.expandAll),g(2),D("ngIf",-1===n.settings.grouping),g(1),D("ngIf",0===n.settings.grouping),g(1),D("ngIf",n.settings.grouping>0),g(2),oe(" ",n.translations.grouping," "),g(1),D("max",n.settings.groupingMaximum)("ngModel",n.settings.grouping),g(2),D("ngIf",n.historicCoverageExecutionTimes.length>0),g(3),oe("",n.translations.filter," "),g(1),D("ngModel",n.settings.filter),g(10),D("ngIf",n.branchCoverageAvailable),g(1),D("ngIf",n.branchCoverageAvailable),g(1),D("ngIf",n.branchCoverageAvailable),g(1),D("ngIf",n.branchCoverageAvailable),g(5),D("ngClass",st(31,In,"name"===n.settings.sortBy&&"desc"===n.settings.sortOrder,"name"===n.settings.sortBy&&"asc"===n.settings.sortOrder,"name"!==n.settings.sortBy)),g(1),P(n.translations.name),g(3),D("ngClass",st(35,In,"covered"===n.settings.sortBy&&"desc"===n.settings.sortOrder,"covered"===n.settings.sortBy&&"asc"===n.settings.sortOrder,"covered"!==n.settings.sortBy)),g(1),P(n.translations.covered),g(3),D("ngClass",st(39,In,"uncovered"===n.settings.sortBy&&"desc"===n.settings.sortOrder,"uncovered"===n.settings.sortBy&&"asc"===n.settings.sortOrder,"uncovered"!==n.settings.sortBy)),g(1),P(n.translations.uncovered),g(3),D("ngClass",st(43,In,"coverable"===n.settings.sortBy&&"desc"===n.settings.sortOrder,"coverable"===n.settings.sortBy&&"asc"===n.settings.sortOrder,"coverable"!==n.settings.sortBy)),g(1),P(n.translations.coverable),g(3),D("ngClass",st(47,In,"total"===n.settings.sortBy&&"desc"===n.settings.sortOrder,"total"===n.settings.sortBy&&"asc"===n.settings.sortOrder,"total"!==n.settings.sortBy)),g(1),P(n.translations.total),g(3),D("ngClass",st(51,In,"coverage"===n.settings.sortBy&&"desc"===n.settings.sortOrder,"coverage"===n.settings.sortBy&&"asc"===n.settings.sortOrder,"coverage"!==n.settings.sortBy)),g(1),P(n.translations.coverage),g(1),D("ngIf",n.branchCoverageAvailable),g(1),D("ngIf",n.branchCoverageAvailable),g(1),D("ngIf",n.branchCoverageAvailable),g(2),D("ngForOf",n.codeElements)}}let _O=(()=>{class e{constructor(n){this.queryString="",this.historicCoverageExecutionTimes=[],this.branchCoverageAvailable=!1,this.codeElements=[],this.translations={},this.settings=new iF,this.window=n.nativeWindow}ngOnInit(){this.historicCoverageExecutionTimes=this.window.historicCoverageExecutionTimes,this.branchCoverageAvailable=this.window.branchCoverageAvailable,this.translations=this.window.translations;let n=!1;if(void 0!==this.window.history&&void 0!==this.window.history.replaceState&&null!==this.window.history.state&&null!=this.window.history.state.coverageInfoSettings)console.log("Coverage info: Restoring from history",this.window.history.state.coverageInfoSettings),n=!0,this.settings=JSON.parse(JSON.stringify(this.window.history.state.coverageInfoSettings));else{let o=0,i=this.window.assemblies;for(let s=0;s<i.length;s++)for(let a=0;a<i[s].classes.length;a++)o=Math.max(o,(i[s].classes[a].name.match(/\./g)||[]).length);this.settings.groupingMaximum=o,console.log("Grouping maximum: "+o)}const r=window.location.href.indexOf("?");r>-1&&(this.queryString=window.location.href.substr(r)),this.updateCoverageInfo(),n&&this.restoreCollapseState()}onDonBeforeUnlodad(){if(this.saveCollapseState(),void 0!==this.window.history&&void 0!==this.window.history.replaceState){console.log("Coverage info: Updating history",this.settings);let n=new lv;null!==window.history.state&&(n=JSON.parse(JSON.stringify(this.window.history.state))),n.coverageInfoSettings=JSON.parse(JSON.stringify(this.settings)),window.history.replaceState(n,"")}}updateCoverageInfo(){let n=(new Date).getTime(),r=this.window.assemblies,o=[],i=0;if(0===this.settings.grouping)for(let l=0;l<r.length;l++){let c=new qn(r[l].name,null);o.push(c);for(let u=0;u<r[l].classes.length;u++)c.insertClass(new Vd(r[l].classes[u],this.queryString),null),i++}else if(-1===this.settings.grouping){let l=new qn(this.translations.all,null);o.push(l);for(let c=0;c<r.length;c++)for(let u=0;u<r[c].classes.length;u++)l.insertClass(new Vd(r[c].classes[u],this.queryString),null),i++}else for(let l=0;l<r.length;l++){let c=new qn(r[l].name,null);o.push(c);for(let u=0;u<r[l].classes.length;u++)c.insertClass(new Vd(r[l].classes[u],this.queryString),this.settings.grouping),i++}let s=-1,a=1;"name"===this.settings.sortBy&&(s="asc"===this.settings.sortOrder?-1:1,a="asc"===this.settings.sortOrder?1:-1),o.sort(function(l,c){return l.name===c.name?0:l.name<c.name?s:a}),qn.sortCodeElementViewModels(o,this.settings.sortBy,"asc"===this.settings.sortOrder);for(let l=0;l<o.length;l++)o[l].changeSorting(this.settings.sortBy,"asc"===this.settings.sortOrder);this.codeElements=o,console.log(`Processing assemblies finished (Duration: ${(new Date).getTime()-n}ms, Assemblies: ${o.length}, Classes: ${i})`),""!==this.settings.historyComparisionDate&&this.updateCurrentHistoricCoverage()}updateCurrentHistoricCoverage(){let n=(new Date).getTime();for(let r=0;r<this.codeElements.length;r++)this.codeElements[r].updateCurrentHistoricCoverage(this.settings.historyComparisionDate);console.log(`Updating current historic coverage finished (Duration: ${(new Date).getTime()-n}ms)`)}collapseAll(n){n.preventDefault();for(let r=0;r<this.codeElements.length;r++)this.codeElements[r].collapse()}expandAll(n){n.preventDefault();for(let r=0;r<this.codeElements.length;r++)this.codeElements[r].expand()}updateSorting(n,r){r.preventDefault(),this.settings.sortOrder=n===this.settings.sortBy&&"asc"===this.settings.sortOrder?"desc":"asc",this.settings.sortBy=n,console.log(`Updating sort column: '${this.settings.sortBy}' (${this.settings.sortOrder})`),qn.sortCodeElementViewModels(this.codeElements,this.settings.sortBy,"asc"===this.settings.sortOrder);for(let o=0;o<this.codeElements.length;o++)this.codeElements[o].changeSorting(this.settings.sortBy,"asc"===this.settings.sortOrder)}saveCollapseState(){this.settings.collapseStates=[];let n=r=>{for(let o=0;o<r.length;o++)this.settings.collapseStates.push(r[o].collapsed),n(r[o].subElements)};n(this.codeElements)}restoreCollapseState(){let n=0,r=o=>{for(let i=0;i<o.length;i++)this.settings.collapseStates.length>n&&(o[i].collapsed=this.settings.collapseStates[n]),n++,r(o[i].subElements)};r(this.codeElements)}}return e.\u0275fac=function(n){return new(n||e)(I(kd))},e.\u0275cmp=xn({type:e,selectors:[["coverage-info"]],hostBindings:function(n,r){1&n&&Z("beforeunload",function(){return r.onDonBeforeUnlodad()},!1,Hl)},decls:1,vars:1,consts:[[4,"ngIf"],[1,"customizebox"],["href","#",3,"click"],[1,"center"],["type","range","step","1","min","-1",3,"max","ngModel","ngModelChange"],[1,"right"],["type","text",3,"ngModel","ngModelChange"],[1,"overview","table-fixed","stripped"],[1,"column90"],[1,"column105"],[1,"column100"],[1,"column70"],[1,"column98"],[1,"column112"],["class","column90",4,"ngIf"],["class","column70",4,"ngIf"],["class","column98",4,"ngIf"],["class","column112",4,"ngIf"],[1,"icon-down-dir",3,"ngClass"],["colspan","2",1,"center"],["class","right",4,"ngIf"],["class","center","colspan","2",4,"ngIf"],[4,"ngFor","ngForOf"],[3,"ngModel","ngModelChange"],["value",""],[3,"value",4,"ngFor","ngForOf"],[3,"value"],["value","allChanges"],["value","lineCoverageIncreaseOnly"],["value","lineCoverageDecreaseOnly"],["value","branchCoverageIncreaseOnly",4,"ngIf"],["value","branchCoverageDecreaseOnly",4,"ngIf"],["value","branchCoverageIncreaseOnly"],["value","branchCoverageDecreaseOnly"],["codeelement-row","",3,"element","collapsed","branchCoverageAvailable",4,"ngIf"],["codeelement-row","",3,"element","collapsed","branchCoverageAvailable"],["class-row","",3,"clazz","translations","branchCoverageAvailable","historyComparisionDate",4,"ngIf"],["class-row","",3,"clazz","translations","branchCoverageAvailable","historyComparisionDate"],["codeelement-row","",1,"namespace",3,"element","collapsed","branchCoverageAvailable"],["class","namespace","class-row","",3,"clazz","translations","branchCoverageAvailable","historyComparisionDate",4,"ngIf"],["class-row","",1,"namespace",3,"clazz","translations","branchCoverageAvailable","historyComparisionDate"]],template:function(n,r){1&n&&S(0,mO,65,55,"div",0),2&n&&D("ngIf",r.codeElements.length>0)},directives:[yo,Td,Pi,gd,Ba,Ni,Yu,Hi,Rd,Od,_F,zF],encapsulation:2}),e})();class yO{constructor(){this.assembly="",this.numberOfRiskHotspots=10,this.filter="",this.sortBy="",this.sortOrder="asc"}}function CO(e,t){if(1&e&&(y(0,"option",14),M(1),C()),2&e){const n=t.$implicit;D("value",n),g(1),P(n)}}function vO(e,t){if(1&e&&(y(0,"span"),M(1),C()),2&e){const n=w(2);g(1),P(n.translations.top)}}function DO(e,t){1&e&&(y(0,"option",21),M(1,"20"),C())}function bO(e,t){1&e&&(y(0,"option",22),M(1,"50"),C())}function EO(e,t){1&e&&(y(0,"option",23),M(1,"100"),C())}function wO(e,t){if(1&e&&(y(0,"option",14),M(1),C()),2&e){const n=w(3);D("value",n.totalNumberOfRiskHotspots),g(1),P(n.translations.all)}}function IO(e,t){if(1&e){const n=ln();y(0,"select",15),Z("ngModelChange",function(o){return le(n),w(2).settings.numberOfRiskHotspots=o}),y(1,"option",16),M(2,"10"),C(),S(3,DO,2,0,"option",17),S(4,bO,2,0,"option",18),S(5,EO,2,0,"option",19),S(6,wO,2,2,"option",20),C()}if(2&e){const n=w(2);D("ngModel",n.settings.numberOfRiskHotspots),g(3),D("ngIf",n.totalNumberOfRiskHotspots>10),g(1),D("ngIf",n.totalNumberOfRiskHotspots>20),g(1),D("ngIf",n.totalNumberOfRiskHotspots>50),g(1),D("ngIf",n.totalNumberOfRiskHotspots>100)}}function MO(e,t){1&e&&k(0,"col",24)}const Ha=function(e,t,n){return{"icon-up-dir_active":e,"icon-down-dir_active":t,"icon-down-dir":n}};function TO(e,t){if(1&e){const n=ln();y(0,"th"),y(1,"a",11),Z("click",function(o){const s=le(n).index;return w(2).updateSorting(""+s,o)}),k(2,"i",12),M(3),C(),y(4,"a",25),k(5,"i",26),C(),C()}if(2&e){const n=t.$implicit,r=t.index,o=w(2);g(2),D("ngClass",st(3,Ha,o.settings.sortBy===""+r&&"desc"===o.settings.sortOrder,o.settings.sortBy===""+r&&"asc"===o.settings.sortOrder,o.settings.sortBy!==""+r)),g(1),P(n.name),g(1),oi("href",n.explanationUrl,Vr)}}const AO=function(e,t){return{lightred:e,lightgreen:t}};function SO(e,t){if(1&e&&(y(0,"td",29),M(1),C()),2&e){const n=t.$implicit;D("ngClass",su(2,AO,n.exceeded,!n.exceeded)),g(1),P(n.value)}}function xO(e,t){if(1&e&&(y(0,"tr"),y(1,"td"),M(2),C(),y(3,"td"),y(4,"a",25),M(5),C(),C(),y(6,"td",27),y(7,"a",25),M(8),C(),C(),S(9,SO,2,5,"td",28),C()),2&e){const n=t.$implicit,r=w(2);g(2),P(n.assembly),g(2),D("href",n.reportPath+r.queryString,Vr),g(1),P(n.class),g(1),D("title",n.methodName),g(1),D("href",n.reportPath+r.queryString+"#file"+n.fileIndex+"_line"+n.line,Vr),g(1),oe(" ",n.methodShortName," "),g(1),D("ngForOf",n.metrics)}}function NO(e,t){if(1&e){const n=ln();y(0,"div"),y(1,"div",1),y(2,"div"),y(3,"select",2),Z("ngModelChange",function(o){return le(n),w().settings.assembly=o})("ngModelChange",function(){return le(n),w().updateRiskHotpots()}),y(4,"option",3),M(5),C(),S(6,CO,2,2,"option",4),C(),C(),y(7,"div",5),S(8,vO,2,1,"span",0),S(9,IO,7,5,"select",6),C(),k(10,"div",5),y(11,"div",7),y(12,"span"),M(13),C(),y(14,"input",8),Z("ngModelChange",function(o){return le(n),w().settings.filter=o})("ngModelChange",function(){return le(n),w().updateRiskHotpots()}),C(),C(),C(),y(15,"table",9),y(16,"colgroup"),k(17,"col"),k(18,"col"),k(19,"col"),S(20,MO,1,0,"col",10),C(),y(21,"thead"),y(22,"tr"),y(23,"th"),y(24,"a",11),Z("click",function(o){return le(n),w().updateSorting("assembly",o)}),k(25,"i",12),M(26),C(),C(),y(27,"th"),y(28,"a",11),Z("click",function(o){return le(n),w().updateSorting("class",o)}),k(29,"i",12),M(30),C(),C(),y(31,"th"),y(32,"a",11),Z("click",function(o){return le(n),w().updateSorting("method",o)}),k(33,"i",12),M(34),C(),C(),S(35,TO,6,7,"th",13),C(),C(),y(36,"tbody"),S(37,xO,10,7,"tr",13),function(e,t){const n=J();let r;const o=e+20;n.firstCreatePass?(r=function(e,t){if(t)for(let n=t.length-1;n>=0;n--){const r=t[n];if(e===r.name)return r}throw new Qn("302",`The pipe '${e}' could not be found!`)}(t,n.pipeRegistry),n.data[o]=r,r.onDestroy&&(n.destroyHooks||(n.destroyHooks=[])).push(o,r.onDestroy)):r=n.data[o];const i=r.factory||(r.factory=Xn(r.type)),s=An(I);try{const a=us(!1),l=i();us(a),function(e,t,n,r){n>=e.data.length&&(e.data[n]=null,e.blueprint[n]=null),t[n]=r}(n,b(),o,l)}finally{An(s)}}(38,"slice"),C(),C(),C()}if(2&e){const n=w();g(3),D("ngModel",n.settings.assembly),g(2),P(n.translations.assembly),g(1),D("ngForOf",n.assemblies),g(2),D("ngIf",n.totalNumberOfRiskHotspots>10),g(1),D("ngIf",n.totalNumberOfRiskHotspots>10),g(4),oe("",n.translations.filter," "),g(1),D("ngModel",n.settings.filter),g(6),D("ngForOf",n.riskHotspotMetrics),g(5),D("ngClass",st(20,Ha,"assembly"===n.settings.sortBy&&"desc"===n.settings.sortOrder,"assembly"===n.settings.sortBy&&"asc"===n.settings.sortOrder,"assembly"!==n.settings.sortBy)),g(1),P(n.translations.assembly),g(3),D("ngClass",st(24,Ha,"class"===n.settings.sortBy&&"desc"===n.settings.sortOrder,"class"===n.settings.sortBy&&"asc"===n.settings.sortOrder,"class"!==n.settings.sortBy)),g(1),P(n.translations.class),g(3),D("ngClass",st(28,Ha,"method"===n.settings.sortBy&&"desc"===n.settings.sortOrder,"method"===n.settings.sortBy&&"asc"===n.settings.sortOrder,"method"!==n.settings.sortBy)),g(1),P(n.translations.method),g(1),D("ngForOf",n.riskHotspotMetrics),g(2),D("ngForOf",M_(38,16,n.riskHotspots,0,n.settings.numberOfRiskHotspots))}}let RO=(()=>{class e{constructor(n){this.queryString="",this.riskHotspotMetrics=[],this.riskHotspots=[],this.totalNumberOfRiskHotspots=0,this.assemblies=[],this.translations={},this.settings=new yO,this.window=n.nativeWindow}ngOnInit(){this.riskHotspotMetrics=this.window.riskHotspotMetrics,this.translations=this.window.translations,void 0!==this.window.history&&void 0!==this.window.history.replaceState&&null!==this.window.history.state&&null!=this.window.history.state.riskHotspotsSettings&&(console.log("Risk hotspots: Restoring from history",this.window.history.state.riskHotspotsSettings),this.settings=JSON.parse(JSON.stringify(this.window.history.state.riskHotspotsSettings)));const n=window.location.href.indexOf("?");n>-1&&(this.queryString=window.location.href.substr(n)),this.updateRiskHotpots()}onDonBeforeUnlodad(){if(void 0!==this.window.history&&void 0!==this.window.history.replaceState){console.log("Risk hotspots: Updating history",this.settings);let n=new lv;null!==window.history.state&&(n=JSON.parse(JSON.stringify(this.window.history.state))),n.riskHotspotsSettings=JSON.parse(JSON.stringify(this.settings)),window.history.replaceState(n,"")}}updateRiskHotpots(){const n=this.window.riskHotspots;if(this.totalNumberOfRiskHotspots=n.length,0===this.assemblies.length){let s=[];for(let a=0;a<n.length;a++)-1===s.indexOf(n[a].assembly)&&s.push(n[a].assembly);this.assemblies=s.sort()}let r=[];for(let s=0;s<n.length;s++)""!==this.settings.filter&&-1===n[s].class.toLowerCase().indexOf(this.settings.filter)||""!==this.settings.assembly&&n[s].assembly!==this.settings.assembly||r.push(n[s]);let o="asc"===this.settings.sortOrder?-1:1,i="asc"===this.settings.sortOrder?1:-1;if("assembly"===this.settings.sortBy)r.sort(function(s,a){return s.assembly===a.assembly?0:s.assembly<a.assembly?o:i});else if("class"===this.settings.sortBy)r.sort(function(s,a){return s.class===a.class?0:s.class<a.class?o:i});else if("method"===this.settings.sortBy)r.sort(function(s,a){return s.methodShortName===a.methodShortName?0:s.methodShortName<a.methodShortName?o:i});else if(""!==this.settings.sortBy){let s=parseInt(this.settings.sortBy,10);r.sort(function(a,l){return a.metrics[s].value===l.metrics[s].value?0:a.metrics[s].value<l.metrics[s].value?o:i})}this.riskHotspots=r}updateSorting(n,r){r.preventDefault(),this.settings.sortOrder=n===this.settings.sortBy&&"asc"===this.settings.sortOrder?"desc":"asc",this.settings.sortBy=n,console.log(`Updating sort column: '${this.settings.sortBy}' (${this.settings.sortOrder})`),this.updateRiskHotpots()}}return e.\u0275fac=function(n){return new(n||e)(I(kd))},e.\u0275cmp=xn({type:e,selectors:[["risk-hotspots"]],hostBindings:function(n,r){1&n&&Z("beforeunload",function(){return r.onDonBeforeUnlodad()},!1,Hl)},decls:1,vars:1,consts:[[4,"ngIf"],[1,"customizebox"],["name","assembly",3,"ngModel","ngModelChange"],["value",""],[3,"value",4,"ngFor","ngForOf"],[1,"center"],[3,"ngModel","ngModelChange",4,"ngIf"],[1,"right"],["type","text",3,"ngModel","ngModelChange"],[1,"overview","table-fixed","stripped"],["class","column105",4,"ngFor","ngForOf"],["href","#",3,"click"],[1,"icon-down-dir",3,"ngClass"],[4,"ngFor","ngForOf"],[3,"value"],[3,"ngModel","ngModelChange"],["value","10"],["value","20",4,"ngIf"],["value","50",4,"ngIf"],["value","100",4,"ngIf"],[3,"value",4,"ngIf"],["value","20"],["value","50"],["value","100"],[1,"column105"],[3,"href"],[1,"icon-info-circled"],[3,"title"],["class","right",3,"ngClass",4,"ngFor","ngForOf"],[1,"right",3,"ngClass"]],template:function(n,r){1&n&&S(0,NO,39,32,"div",0),2&n&&D("ngIf",r.totalNumberOfRiskHotspots>0)},directives:[yo,Hi,gd,Ba,Rd,Od,Yu,Pi,Ni],pipes:[Yy],encapsulation:2}),e})(),FO=(()=>{class e{}return e.\u0275fac=function(n){return new(n||e)},e.\u0275mod=mn({type:e,bootstrap:[RO,_O]}),e.\u0275inj=Ft({providers:[kd],imports:[[iR,oF]]}),e})();rR().bootstrapModule(FO).catch(e=>console.error(e))}},wo=>{wo(wo.s=15)}]);