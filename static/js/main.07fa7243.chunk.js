(window.webpackJsonp=window.webpackJsonp||[]).push([[0],[,function(){!function(n){"use strict";function e(n,e,r){return r.a=n,r.f=e,r}function r(n){return e(2,n,function(e){return function(r){return n(e,r)}})}function t(n){return e(3,n,function(e){return function(r){return function(t){return n(e,r,t)}}})}function o(n){return e(4,n,function(e){return function(r){return function(t){return function(o){return n(e,r,t,o)}}}})}function a(n,e,r){return 2===n.a?n.f(e,r):n(e)(r)}function u(n,e,r,t){return 3===n.a?n.f(e,r,t):n(e)(r)(t)}function i(n,e,r,t,o){return 4===n.a?n.f(e,r,t,o):n(e)(r)(t)(o)}var c=t(function(n,e,r){for(var t=Array(n),o=0;o<n;o++)t[o]=r(e+o);return t}),f=r(function(n,e){for(var r=Array(n),t=0;t<n&&e.b;t++)r[t]=e.a,e=e.b;return r.length=t,m(r,e)}),s={$:0};function l(n,e){return{$:1,a:n,b:e}}var d=r(l);function h(n){for(var e=s,r=n.length;r--;)e=l(n[r],e);return e}function v(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}function b(n,e,r,t){if(r>100)return t.push(m(n,e)),!0;if(n===e)return!0;if("object"!==typeof n||null===n||null===e)return"function"===typeof n&&v(5),!1;for(var o in n.$<0&&(n=Fn(n),e=Fn(e)),n)if(!b(n[o],e[o],r+1,t))return!1;return!0}function g(n,e,r){if("object"!==typeof n)return n===e?0:n<e?-1:1;if("undefined"===typeof n.$)return(r=g(n.a,e.a))?r:(r=g(n.b,e.b))?r:g(n.c,e.c);for(;n.b&&e.b&&!(r=g(n.a,e.a));n=n.b,e=e.b);return r||(n.b?1:e.b?-1:0)}function m(n,e){return{a:n,b:e}}function p(n,e){var r={};for(var t in n)r[t]=n[t];for(var t in e)r[t]=e[t];return r}function y(n,e){if("string"===typeof n)return n+e;if(!n.b)return e;var r=l(n.a,e);n=n.b;for(var t=r;n.b;n=n.b)t=t.b=l(n.a,e);return r}var w=r(function(n,e){var r=e%n;return 0===n?v(11):r>0&&n<0||r<0&&n>0?r+n:r}),$=Math.ceil,k=Math.floor,A=Math.log,x=r(function(n,e){return e.join(n)});function T(n){return{$:2,b:n}}var j=T(function(n){return"number"!==typeof n?L("an INT",n):-2147483647<n&&n<2147483647&&(0|n)===n?Ae(n):!isFinite(n)||n%1?L("an INT",n):Ae(n)}),S=(T(function(n){return"boolean"===typeof n?Ae(n):L("a BOOL",n)}),T(function(n){return"number"===typeof n?Ae(n):L("a FLOAT",n)}),T(function(n){return Ae(R(n))}),T(function(n){return"string"===typeof n?Ae(n):n instanceof String?Ae(n+""):L("a STRING",n)})),N=r(function(n,e){return{$:6,d:n,b:e}});var _=r(function(n,e){return{$:10,b:e,h:n}}),C=r(function(n,e){return function(n,e){return{$:9,f:n,g:e}}(n,[e])}),W=r(function(n,e){return Y(n,F(e))});function Y(n,e){switch(n.$){case 2:return n.b(e);case 5:return null===e?Ae(n.c):L("null",e);case 3:return E(e)?H(n.b,e,h):L("a LIST",e);case 4:return E(e)?H(n.b,e,O):L("an ARRAY",e);case 6:var r=n.d;if("object"!==typeof e||null===e||!(r in e))return L("an OBJECT with a field named `"+r+"`",e);var t=Y(n.b,e[r]);return ae(t)?t:ke(a(Te,r,t.a));case 7:var o=n.e;return E(e)?o<e.length?(t=Y(n.b,e[o]),ae(t)?t:ke(a(je,o,t.a))):L("a LONGER array. Need index "+o+" but only see "+e.length+" entries",e):L("an ARRAY",e);case 8:if("object"!==typeof e||null===e||E(e))return L("an OBJECT",e);var u=s;for(var i in e)if(e.hasOwnProperty(i)){if(t=Y(n.b,e[i]),!ae(t))return ke(a(Te,i,t.a));u=l(m(i,t.a),u)}return Ae(Mn(u));case 9:for(var c=n.f,f=n.g,d=0;d<f.length;d++){if(t=Y(f[d],e),!ae(t))return t;c=c(t.a)}return Ae(c);case 10:return t=Y(n.b,e),ae(t)?Y(n.h(t.a),e):t;case 11:for(var v=s,b=n.g;b.b;b=b.b){if(t=Y(b.a,e),ae(t))return t;v=l(t.a,v)}return ke(Se(Mn(v)));case 1:return ke(a(xe,n.a,R(e)));case 0:return Ae(n.a)}}function H(n,e,r){for(var t=e.length,o=Array(t),u=0;u<t;u++){var i=Y(n,e[u]);if(!ae(i))return ke(a(je,u,i.a));o[u]=i.a}return Ae(r(o))}function E(n){return Array.isArray(n)||"function"===typeof FileList&&n instanceof FileList}function O(n){return a($e,n.length,function(e){return n[e]})}function L(n,e){return ke(a(xe,"Expecting "+n,R(e)))}function D(n,e){if(n===e)return!0;if(n.$!==e.$)return!1;switch(n.$){case 0:case 1:return n.a===e.a;case 2:return n.b===e.b;case 5:return n.c===e.c;case 3:case 4:case 8:return D(n.b,e.b);case 6:return n.d===e.d&&D(n.b,e.b);case 7:return n.e===e.e&&D(n.b,e.b);case 9:return n.f===e.f&&q(n.g,e.g);case 10:return n.h===e.h&&D(n.b,e.b);case 11:return q(n.g,e.g)}}function q(n,e){var r=n.length;if(r!==e.length)return!1;for(var t=0;t<r;t++)if(!D(n[t],e[t]))return!1;return!0}function R(n){return n}function F(n){return n}function P(n){return{$:0,a:n}}function z(n){return{$:2,b:n,c:null}}R(null);var I=r(function(n,e){return{$:3,b:n,d:e}}),B=0;function M(n){var e={$:0,e:B++,f:n,g:null,h:[]};return U(e),e}var J=!1,X=[];function U(n){if(X.push(n),!J){for(J=!0;n=X.shift();)G(n);J=!1}}function G(n){for(;n.f;){var e=n.f.$;if(0===e||1===e){for(;n.g&&n.g.$!==e;)n.g=n.g.i;if(!n.g)return;n.f=n.g.b(n.f.a),n.g=n.g.i}else{if(2===e)return void(n.f.c=n.f.b(function(e){n.f=e,U(n)}));if(5===e){if(0===n.h.length)return;n.f=n.f.b(n.h.shift())}else n.g={$:3===e?0:1,b:n.f.b,i:n.g},n.f=n.f.d}}}var K={};function V(n,e){var r={g:e,h:void 0},t=n.c,o=n.d,c=n.e,f=n.f;return r.h=M(a(I,function n(e){return a(I,n,{$:5,b:function(n){var a=n.a;return 0===n.$?u(o,r,a,e):c&&f?i(t,r,a.i,a.j,e):u(t,r,c?a.i:a.j,e)}})},n.b))}var Z,Q=r(function(n,e){return z(function(r){n.g(e),r(P(0))})});function nn(n){return{$:2,m:n}}function en(n,e,r){var t,o={};for(var a in rn(!0,e,o,null),rn(!1,r,o,null),n)(t=n[a]).h.push({$:"fx",a:o[a]||{i:s,j:s}}),U(t)}function rn(n,e,r,t){switch(e.$){case 1:var o=e.k,u=function(n,r,t){return a(n?K[r].e:K[r].f,function(n){for(var e=t;e;e=e.q)n=e.p(n);return n},e.l)}(n,o,t);return void(r[o]=function(n,e,r){return r=r||{i:s,j:s},n?r.i=l(e,r.i):r.j=l(e,r.j),r}(n,u,r[o]));case 2:for(var i=e.m;i.b;i=i.b)rn(n,i.a,r,t);return;case 3:return void rn(n,e.o,r,{p:e.n,q:t})}}var tn="undefined"!==typeof document?document:{};function on(n,e){n.appendChild(e)}function an(n){return{$:0,a:n}}var un=r(function(n,e){return r(function(r,t){for(var o=[],a=0;t.b;t=t.b){var u=t.a;a+=u.b||0,o.push(u)}return a+=o.length,{$:1,c:e,d:hn(r),e:o,f:n,b:a}})})(void 0);r(function(n,e){return r(function(r,t){for(var o=[],a=0;t.b;t=t.b){var u=t.a;a+=u.b.b||0,o.push(u)}return a+=o.length,{$:2,c:e,d:hn(r),e:o,f:n,b:a}})})(void 0);var cn,fn=r(function(n,e){return{$:"a0",n:n,o:e}}),sn=r(function(n,e){return{$:"a1",n:n,o:e}}),ln=r(function(n,e){return{$:"a2",n:n,o:e}}),dn=r(function(n,e){return{$:"a3",n:n,o:e}});function hn(n){for(var e={};n.b;n=n.b){var r=n.a,t=r.$,o=r.n,a=r.o;if("a2"!==t){var u=e[t]||(e[t]={});"a3"===t&&"class"===o?vn(u,o,a):u[o]=a}else"className"===o?vn(e,o,F(a)):e[o]=F(a)}return e}function vn(n,e,r){var t=n[e];n[e]=t?t+" "+r:r}function bn(n,e){var r=n.$;if(5===r)return bn(n.k||(n.k=n.m()),e);if(0===r)return tn.createTextNode(n.a);if(4===r){for(var t=n.k,o=n.j;4===t.$;)"object"!==typeof o?o=[o,t.j]:o.push(t.j),t=t.k;var a={j:o,p:e};return(u=bn(t,a)).elm_event_node_ref=a,u}if(3===r)return gn(u=n.h(n.g),e,n.d),u;var u=n.f?tn.createElementNS(n.f,n.c):tn.createElement(n.c);Z&&"a"==n.c&&u.addEventListener("click",Z(u)),gn(u,e,n.d);for(var i=n.e,c=0;c<i.length;c++)on(u,bn(1===r?i[c]:i[c].b,e));return u}function gn(n,e,r){for(var t in r){var o=r[t];"a1"===t?mn(n,o):"a0"===t?wn(n,e,o):"a3"===t?pn(n,o):"a4"===t?yn(n,o):("value"!==t&&"checked"!==t||n[t]!==o)&&(n[t]=o)}}function mn(n,e){var r=n.style;for(var t in e)r[t]=e[t]}function pn(n,e){for(var r in e){var t=e[r];"undefined"!==typeof t?n.setAttribute(r,t):n.removeAttribute(r)}}function yn(n,e){for(var r in e){var t=e[r],o=t.f,a=t.o;"undefined"!==typeof a?n.setAttributeNS(o,r,a):n.removeAttributeNS(o,r)}}function wn(n,e,r){var t=n.elmFs||(n.elmFs={});for(var o in r){var a=r[o],u=t[o];if(a){if(u){if(u.q.$===a.$){u.q=a;continue}n.removeEventListener(o,u)}u=$n(e,a),n.addEventListener(o,u,cn&&{passive:vr(a)<2}),t[o]=u}else n.removeEventListener(o,u),t[o]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){cn=!0}}))}catch(n){}function $n(n,e){function r(e){var t=r.q,o=Y(t.a,e);if(ae(o)){for(var a,u=vr(t),i=o.a,c=u?u<3?i.a:i.F:i,f=1==u?i.b:3==u&&i.aD,s=(f&&e.stopPropagation(),(2==u?i.b:3==u&&i.aB)&&e.preventDefault(),n);a=s.j;){if("function"==typeof a)c=a(c);else for(var l=a.length;l--;)c=a[l](c);s=s.p}s(c,f)}}return r.q=e,r}function kn(n,e){return n.$==e.$&&D(n.a,e.a)}function An(n,e,r,t){var o={$:e,r:r,s:t,t:void 0,u:void 0};return n.push(o),o}function xn(n,e,r,t){if(n!==e){var o=n.$,a=e.$;if(o!==a){if(1!==o||2!==a)return void An(r,0,t,e);e=function(n){for(var e=n.e,r=e.length,t=Array(r),o=0;o<r;o++)t[o]=e[o].b;return{$:1,c:n.c,d:n.d,e:t,f:n.f,b:n.b}}(e),a=1}switch(a){case 5:for(var u=n.l,i=e.l,c=u.length,f=c===i.length;f&&c--;)f=u[c]===i[c];if(f)return void(e.k=n.k);e.k=e.m();var s=[];return xn(n.k,e.k,s,0),void(s.length>0&&An(r,1,t,s));case 4:for(var l=n.j,d=e.j,h=!1,v=n.k;4===v.$;)h=!0,"object"!==typeof l?l=[l,v.j]:l.push(v.j),v=v.k;for(var b=e.k;4===b.$;)h=!0,"object"!==typeof d?d=[d,b.j]:d.push(b.j),b=b.k;return h&&l.length!==d.length?void An(r,0,t,e):((h?function(n,e){for(var r=0;r<n.length;r++)if(n[r]!==e[r])return!1;return!0}(l,d):l===d)||An(r,2,t,d),void xn(v,b,r,t+1));case 0:return void(n.a!==e.a&&An(r,3,t,e.a));case 1:return void Tn(n,e,r,t,Sn);case 2:return void Tn(n,e,r,t,Nn);case 3:if(n.h!==e.h)return void An(r,0,t,e);var g=jn(n.d,e.d);g&&An(r,4,t,g);var m=e.i(n.g,e.g);return void(m&&An(r,5,t,m))}}}function Tn(n,e,r,t,o){if(n.c===e.c&&n.f===e.f){var a=jn(n.d,e.d);a&&An(r,4,t,a),o(n,e,r,t)}else An(r,0,t,e)}function jn(n,e,r){var t;for(var o in n)if("a1"!==o&&"a0"!==o&&"a3"!==o&&"a4"!==o)if(o in e){var a=n[o],u=e[o];a===u&&"value"!==o&&"checked"!==o||"a0"===r&&kn(a,u)||((t=t||{})[o]=u)}else(t=t||{})[o]=r?"a1"===r?"":"a0"===r||"a3"===r?void 0:{f:n[o].f,o:void 0}:"string"===typeof n[o]?"":null;else{var i=jn(n[o],e[o]||{},o);i&&((t=t||{})[o]=i)}for(var c in e)c in n||((t=t||{})[c]=e[c]);return t}function Sn(n,e,r,t){var o=n.e,a=e.e,u=o.length,i=a.length;u>i?An(r,6,t,{v:i,i:u-i}):u<i&&An(r,7,t,{v:u,e:a});for(var c=u<i?u:i,f=0;f<c;f++){var s=o[f];xn(s,a[f],r,++t),t+=s.b||0}}function Nn(n,e,r,t){for(var o=[],a={},u=[],i=n.e,c=e.e,f=i.length,s=c.length,l=0,d=0,h=t;l<f&&d<s;){var v=(j=i[l]).a,b=(S=c[d]).a,g=j.b,m=S.b,p=void 0,y=void 0;if(v!==b){var w=i[l+1],$=c[d+1];if(w){var k=w.a,A=w.b;y=b===k}if($){var x=$.a,T=$.b;p=v===x}if(p&&y)xn(g,T,o,++h),Cn(a,o,v,m,d,u),h+=g.b||0,Wn(a,o,v,A,++h),h+=A.b||0,l+=2,d+=2;else if(p)h++,Cn(a,o,b,m,d,u),xn(g,T,o,h),h+=g.b||0,l+=1,d+=2;else if(y)Wn(a,o,v,g,++h),h+=g.b||0,xn(A,m,o,++h),h+=A.b||0,l+=2,d+=1;else{if(!w||k!==x)break;Wn(a,o,v,g,++h),Cn(a,o,b,m,d,u),h+=g.b||0,xn(A,T,o,++h),h+=A.b||0,l+=2,d+=2}}else xn(g,m,o,++h),h+=g.b||0,l++,d++}for(;l<f;){var j;Wn(a,o,(j=i[l]).a,g=j.b,++h),h+=g.b||0,l++}for(;d<s;){var S,N=N||[];Cn(a,o,(S=c[d]).a,S.b,void 0,N),d++}(o.length>0||u.length>0||N)&&An(r,8,t,{w:o,x:u,y:N})}var _n="_elmW6BL";function Cn(n,e,r,t,o,a){var u=n[r];if(!u)return a.push({r:o,A:u={c:0,z:t,r:o,s:void 0}}),void(n[r]=u);if(1===u.c){a.push({r:o,A:u}),u.c=2;var i=[];return xn(u.z,t,i,u.r),u.r=o,void(u.s.s={w:i,A:u})}Cn(n,e,r+_n,t,o,a)}function Wn(n,e,r,t,o){var a=n[r];if(a){if(0===a.c){a.c=2;var u=[];return xn(t,a.z,u,o),void An(e,9,o,{w:u,A:a})}Wn(n,e,r+_n,t,o)}else{var i=An(e,9,o,void 0);n[r]={c:1,z:t,r:o,s:i}}}function Yn(n,e,r,t){return 0===r.length?n:(function n(e,r,t,o){!function e(r,t,o,a,u,i,c){for(var f=o[a],s=f.r;s===u;){var l=f.$;if(1===l)n(r,t.k,f.s,c);else if(8===l)f.t=r,f.u=c,(d=f.s.w).length>0&&e(r,t,d,0,u,i,c);else if(9===l){f.t=r,f.u=c;var d,h=f.s;h&&(h.A.s=r,(d=h.w).length>0&&e(r,t,d,0,u,i,c))}else f.t=r,f.u=c;if(!(f=o[++a])||(s=f.r)>i)return a}var v=t.$;if(4===v){for(var b=t.k;4===b.$;)b=b.k;return e(r,b,o,a,u+1,i,r.elm_event_node_ref)}for(var g=t.e,m=r.childNodes,p=0;p<g.length;p++){u++;var y=1===v?g[p]:g[p].b,w=u+(y.b||0);if(u<=s&&s<=w&&(!(f=o[a=e(m[p],y,o,a,u,w,c)])||(s=f.r)>i))return a;u=w}return a}(e,r,t,0,0,r.b,o)}(n,e,r,t),Hn(n,r))}function Hn(n,e){for(var r=0;r<e.length;r++){var t=e[r],o=t.t,a=En(o,t);o===n&&(n=a)}return n}function En(n,e){switch(e.$){case 0:return function(n){var r=n.parentNode,t=bn(e.s,e.u);return t.elm_event_node_ref||(t.elm_event_node_ref=n.elm_event_node_ref),r&&t!==n&&r.replaceChild(t,n),t}(n);case 4:return gn(n,e.u,e.s),n;case 3:return n.replaceData(0,n.length,e.s),n;case 1:return Hn(n,e.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=e.s:n.elm_event_node_ref={j:e.s,p:e.u},n;case 6:for(var r=e.s,t=0;t<r.i;t++)n.removeChild(n.childNodes[r.v]);return n;case 7:for(var o=(r=e.s).e,a=n.childNodes[t=r.v];t<o.length;t++)n.insertBefore(bn(o[t],e.u),a);return n;case 9:if(!(r=e.s))return n.parentNode.removeChild(n),n;var u=r.A;return"undefined"!==typeof u.r&&n.parentNode.removeChild(n),u.s=Hn(n,r.w),n;case 8:return function(n,e){var r=e.s,t=function(n,e){if(n){for(var r=tn.createDocumentFragment(),t=0;t<n.length;t++){var o=n[t].A;on(r,2===o.c?o.s:bn(o.z,e.u))}return r}}(r.y,e);n=Hn(n,r.w);for(var o=r.x,a=0;a<o.length;a++){var u=o[a],i=u.A,c=2===i.c?i.s:bn(i.z,e.u);n.insertBefore(c,n.childNodes[u.r])}return t&&on(n,t),n}(n,e);case 5:return e.s(n);default:v(10)}}var On=o(function(n,e,r,t){return function(n,e,r,t,o,u){var i=a(W,n,R(e?e.flags:void 0));ae(i)||v(2);var c={},f=(i=r(i.a)).a,s=u(d,f),l=function(n,e){var r;for(var t in K){var o=K[t];o.a&&((r=r||{})[t]=o.a(t,e)),n[t]=V(o,e)}return r}(c,d);function d(n,e){s(f=(i=a(t,n,f)).a,e),en(c,i.b,o(f))}return en(c,i.b,o(f)),l?{ports:l}:{}}(e,t,n.bh,n.bp,n.bn,function(e,r){var o=n.br,i=t.node,c=function n(e){if(3===e.nodeType)return an(e.textContent);if(1!==e.nodeType)return an("");for(var r=s,t=e.attributes,o=t.length;o--;){var i=t[o];r=l(a(dn,i.name,i.value),r)}var c=e.tagName.toLowerCase(),f=s,d=e.childNodes;for(o=d.length;o--;)f=l(n(d[o]),f);return u(un,c,r,f)}(i);return function(n,e){e(n);var r=0;function t(){r=1===r?0:(Ln(t),e(n),1)}return function(o,a){n=o,a?(e(n),2===r&&(r=1)):(0===r&&Ln(t),r=2)}}(r,function(n){var r=o(n),t=function(n,e){var r=[];return xn(n,e,r,0),r}(c,r);i=Yn(i,c,t,e),c=r})})}),Ln=("undefined"!==typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!==typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)});"undefined"!==typeof document&&document,"undefined"!==typeof window&&window;var Dn,qn=d,Rn=t(function(n,e,r){for(;;){if(-2===r.$)return e;var t=r.d,o=n,a=u(n,r.b,r.c,u(Rn,n,e,r.e));n=o,e=a,r=t}}),Fn=function(n){return u(Rn,t(function(n,e,r){return a(qn,m(n,e),r)}),s,n)},Pn=function(n){return"You begin speaking to a customer named "+n.a+". "+n.d},zn=function(n){return{c:n.c,d:n.d,af:function(){switch(n.b){case 0:return 15;case 1:return 20;case 2:return 30;case 3:return 40;default:return 60}}(),a:n.a,H:0,b:n.b}},In=h([{c:"She has wonderful hair!",d:"She browses your product line.",a:"Carol Cooper-Iardlynoer",b:2},{c:"Just a teenage dirtbag baby.",d:"He keeps his hands in his pockets.",a:"Dennis Demacia",b:1},{c:"An iron maiden.",d:"She strides up to your counter confidently in full plate.",a:"Erica Earful",b:4},{c:"Frankly, my dear, he doesn't give a damn.",d:"He seems like he couldn't care less.",a:"Frank Mann-Free",b:2},{c:"Her clothes are expensive, but well-worn - a wealthy woman who has fallen on harder times.",d:"A prim and proper older lady.",a:"Gertrude Ganderstudies",b:2},{c:"He growls when he speaks, and strange jewellery flashes from under his cloak. He keeps muttering about broken swords and half-things.",d:"A dark and mysterious cloaked figure.",a:"Harold Harbinger",b:4},{c:"On closer inspection, the ears appear to be a form of costume jewellery.",d:"A middle-aged woman with surprisingly pointy ears.",a:"Ingrid Isntmael",b:3},{c:"It is quickly obvious the confidence is just a ruse to hide deep-seated insecurities.",d:"A an attractive, confident man who'll flirt with anyone in the store.",a:"Jerome Jackinthebox",b:1},{c:"She speaks in dark and gravelly voice that tends to make people uncomfortable.",d:"Why is it so chilly in here all of a sudden?",a:"Kyla Killthemall",b:3},{c:"His clothes are tighter than they need to be.",d:"An athletic figure.",a:"Liam Lemonmeringue",b:3},{c:"Looking to make an impression.",d:"Very stylish and fashionable",a:"Marion Mansion",b:4},{c:"A friendly chap who manages to maintain a positive attitude despite his unfortunate conditions.",d:"A beggar has made his way into your store.",a:"Noddy Noboddy",b:0},{c:"She speaks at length about her grandchildren in the East.",d:"A kindly looking old lady.",a:"Olivia Oldbutgold",b:3},{c:"He has numerous bags of colourful spices arrayed inside his jacket.",d:"A fragrant and spicy smell wafts through the store.",a:"Patrick Pleasepassthepepper",b:2},{c:"Dressed thick and warm, as if expecting a cold winter.",d:"A dappled and eye-catching array of clothing.",a:"Quinette Qualityquilt",b:3},{c:"Actually quite friendly and well-mannered, just has a hunch back and a lazy eye.",d:"Creeping in with back hunched, eying the racks suspiciously.",a:"Rawry Ragna-Rock",b:2},{c:"She keeps offering you to try some of her quadruple-ginger cookies.",d:"A plump middle-aged woman.",a:"Samantha Saltoftheearth",b:1},{c:"His responses are brief and his mind appears to be elsewhere.",d:"A quiet gentelman standing off on his own.",a:"Toby Tell-Noboddy",b:1},{c:"She gives the impression that she always gets what she wants, sooner or later...",d:"A large and confident lady dressed in enormous robes of purple and strutting in with purpose.",a:"Ursula Ur",b:4},{c:"He tells all about his hobby as a beekeeper and offers to let you try some honey.",d:"A burn mark scars his face.",a:"Vaughn Vatofacid",b:2},{c:"She's wearing quite a lot of makeup, and flirts with everyone in store.",d:"An attractive young woman.",a:"Wendy Mann-Woo",b:2},{c:"He seems incredibly eager to kill goblins.",d:"A big, burly man covered in tattoos",a:"Xavier Xtraspicy",b:2},{c:"Her voice booms loudly, and yet remains pleasant to the ears.",d:"Her muscles ripple as she walks.",a:"Yennefer Yodalayheehoo",b:3},{c:"He speaks slowly and often seems to be a bit lost in his own world.",d:"An attractive young man with a scraggly beard and reddish eyes.",a:"Zander Zoinkies",b:1}]),Bn=t(function(n,e,r){for(;;){if(!r.b)return e;var t=r.b,o=n,u=a(n,r.a,e);n=o,e=u,r=t}}),Mn=function(n){return u(Bn,qn,s,n)},Jn=o(function(n,e,r,t){if(t.b){var o=t.a,c=t.b;if(c.b){var f=c.a,s=c.b;if(s.b){var l=s.a,d=s.b;if(d.b){var h=d.b;return a(n,o,a(n,f,a(n,l,a(n,d.a,r>500?u(Bn,n,e,Mn(h)):i(Jn,n,e,r+1,h)))))}return a(n,o,a(n,f,a(n,l,e)))}return a(n,o,a(n,f,e))}return a(n,o,e)}return e}),Xn=t(function(n,e,r){return i(Jn,n,e,0,r)}),Un=r(function(n,e){return u(Xn,r(function(e,r){return a(qn,n(e),r)}),s,e)}),Gn=a(Un,zn,In),Kn=zn({c:"The smile was endearing at first, but it starts to get creepy after awhile.",d:"She greets you with a smile.",a:"Abby Aubergine",b:1}),Vn=h([zn({c:"Sleazy looking guy. You'd be willing to sell to him, but probably shouldn't trust anything he sold you.",d:"He eyes your store shiftily.",a:"Bob Bucket",b:1})]),Zn=function(n){return{$:0,a:n}},Qn={x:Zn(Kn),y:Gn,ax:2,aO:6,D:Vn},ne={O:0,P:0,ac:0,ak:0,ao:0},ee={$:1},re=function(n){return n+""},te=function(n){var e=function(n){return m((n/60|0)%24,n%60)}(n),r=e.a,t=e.b;return(r<10?"0":"")+re(r)+":"+(t<10?"0":"")+re(t)},oe=r(function(n,e){return p(e,{N:y(e.N,h([h([te(e.f),n,""])]))})}),ae=function(n){return!n.$},ue=o(function(n,e,r,t){return{$:0,a:n,b:e,c:r,d:t}}),ie=$,ce=r(function(n,e){return A(e)/A(n)}),fe=ie(a(ce,2,32)),se=[],le=i(ue,0,fe,se,se),de=f,he=r(function(n,e){for(;;){var r=a(de,32,n),t=r.b,o=a(qn,{$:0,a:r.a},e);if(!t.b)return Mn(o);n=t,e=o}}),ve=r(function(n,e){for(;;){var r=ie(e/32);if(1===r)return a(de,32,n).a;n=a(he,n,s),e=r}}),be=k,ge=r(function(n,e){return g(n,e)>0?n:e}),me=function(n){return n.length},pe=r(function(n,e){if(e.g){var r=32*e.g,t=be(a(ce,32,r-1)),o=n?Mn(e.j):e.j,u=a(ve,o,e.g);return i(ue,me(e.i)+r,a(ge,5,t*fe),u,e.i)}return i(ue,me(e.i),fe,se,e.i)}),ye=c,we=e(5,Dn=function(n,e,r,t,o){for(;;){if(e<0)return a(pe,!1,{j:t,g:r/32|0,i:o});var i={$:1,a:u(ye,32,e,n)};n=n,e-=32,r=r,t=a(qn,i,t),o=o}},function(n){return function(e){return function(r){return function(t){return function(o){return Dn(n,e,r,t,o)}}}}}),$e=r(function(n,e){if(n>0){var r=n%32;return t=we,o=e,a=n-r-32,i=n,c=s,f=u(ye,r,n-r,e),5===t.a?t.f(o,a,i,c,f):t(o)(a)(i)(c)(f)}var t,o,a,i,c,f;return le}),ke=function(n){return{$:1,a:n}},Ae=function(n){return{$:0,a:n}},xe=r(function(n,e){return{$:3,a:n,b:e}}),Te=r(function(n,e){return{$:0,a:n,b:e}}),je=r(function(n,e){return{$:1,a:n,b:e}}),Se=function(n){return{$:2,a:n}},Ne=r(function(n,e){return a(x,n,function(n){for(var e=[];n.b;n=n.b)e.push(n.a);return e}(e))}),_e=nn(s),Ce=r(function(n,e){return u(Xn,r(function(e,r){return n(e)?a(qn,e,r):r}),s,e)}),We=t(function(n,e,r){return y(a(Ce,function(n){return!function(n,e){for(var r,t=[],o=b(n,e,0,t);o&&(r=t.pop());o=b(r.a,r.b,0,t));return o}(n,r)},n),e.$?s:h([e.a]))}),Ye=r(function(n,e){return p(n,{x:Zn(e),D:u(We,n.D,n.x,e)})}),He=r(function(n,e){return a(oe,Pn(n),p(e,{e:a(Ye,e.e,n)}))}),Ee=r(function(n,e){return y(n,h([e]))}),Oe=function(n){var e=n.x;return e.$?n:p(n,{x:ee,y:a(Ee,n.y,e.a)})},Le=function(n){return n/1440|0},De=r(function(n,e){var r=g(n,e)>-1?1+((n-e)/30|0):0;return m(e+30*r,r)}),qe=zn({c:"They seem wrong, somehow. Like something that shouldn't exist.",d:"You sense a bizarre otherworldly presence.",a:"WRONG",b:0}),Re=function(n){return n.b?m(n.a,n.b.b?n.b:s):m(qe,s)},Fe=r(function(n,e){for(;;){if(e<=0)return n;t=Re((r=n).y),n=p(r,{y:t.b,D:y(r.D,h([t.a]))}),e-=1}var r,t}),Pe=t(function(n,e,r){var t=a(De,n,e);return m(t.a,a(Fe,r,t.b))}),ze=r(function(n,e){var r=u(Pe,n,e.X,e.e);return p(e,{e:r.b,f:n,X:r.a})}),Ie=r(function(n,e){return p(e,{P:e.P+n})}),Be=r(function(n,e){return t=p(function(n){return p(n,{v:a(Ie,(e=n.e.D,u(Bn,r(function(n,e){return e+1}),0,e)+(n.e.x.$?0:1)),n.v)});var e}(a(oe,n,a(ze,1440*Le(e.f)+1020,e))),{e:(o=e.e,p(i=Oe(o),{y:y(i.y,i.D),D:s})),w:1}),a(oe,function(n){return r=(e=n.v).ao,o=e.ak,a=e.P,u=e.O,"Stats for the day: "+((t=e.ac)>0?"You sold items to "+re(t)+" customer(s). This netted you a total turnover of "+re(r)+"gp, "+(o>0?"and a profit of "+re(o)+"gp. ":o?"but you made a loss of "+re(o)+"gp! You should sell items for more than you bought them. ":"but you didn't make any profit! You should sell items for more than you bought them. "):"You didn't sell items to anyone, which is a bit worrying considering that you're a store. ")+(u>0?"You told "+re(e.O)+" customer(s) to fuck right off. ":"")+(a>0?"Your negligence resulted in "+re(e.P)+" customer(s) leaving of their own accord.":"You managed to attend to every customer that entered your store, one way or another.");var e,r,t,o,a,u}(t),t);var t,o,i}),Me=r(function(n,e){return a(ze,n+e.f,e)}),Je=w,Xe=function(n){return a(Je,1440,n)},Ue=r(function(n,e){var r=e+n;return g(Xe(r),1020)>-1||g(Xe(r),Xe(e))<0||g(r-e,1440)>0}),Ge=function(n){return p(n,{e:Oe(n.e)})},Ke=function(n){return function(n){return p(n,{v:(e=n.v,p(e,{O:e.O+1}))});var e}(a(oe,1===(r=(e=n.e).x).$?"Who are you kicking out?":"You tell "+r.a.a+" to fuck off. They get angry, and leave the store, taking "+re(e.ax)+" minutes.",Ge(function(n){return!n.w&&a(Ue,n.e.ax,n.f)?a(Be,"Whilst you tell them to fuck off, the store closes anyway",n):n.e.x.$?n:a(Me,n.e.ax,n)}(n))));var e,r},Ve=function(n){return g(n.H,3)<0?p(n,{af:n.af+(n.af/2|0),H:n.H+1}):p(n,{H:n.H+1})},Ze=r(function(n,e){return e.$?ee:Zn(n(e.a))}),Qe=r(function(n,e){return"The customer, "+n.a+", rejected the sales price of "+re(e.n)+"gp for the "+e.S+", taking "+re(10)+" minutes."}),nr=t(function(n,e,r){return a(Ue,10,r.f)?a(Be,"Unfortunately, before the client can reject your offer, the store closes, and you are forced to shoo them out.",r):a(oe,a(Qe,n,e),a(Me,10,r))}),er=r(function(n,e){return"The customer, "+n.a+", bought 1 "+e.S+" at "+re(e.n)+"gp (cost price "+re(e.T)+"gp), and leaves happy, taking "+re(5)+" minutes."}),rr=r(function(n,e){return"You offered the "+(r=e).S+" for a price of "+re(r.n)+"gp.\n"+a(er,n,e);var r}),tr=t(function(n,e,r){return p(r,{ak:r.ak+(n-e),ao:r.ao+n})}),or=t(function(n,e,r){return p(r,{v:u(tr,n,e,r.v)})}),ar=r(function(n,e){return!e.w&&a(Ue,5,e.f)?a(Be,"The customer looked just about ready to buy the item! But unfortunately, what with the curfew and all, you have to tell them to come back tomorrow.",e):function(n){return p(n,{v:(e=n.v,p(e,{ac:e.ac+1}))});var e}(Ge(u(or,e.l.n,e.l.T,function(n){return p(n,{aj:n.aj+n.l.n})}(a(oe,a(rr,n,e.l),a(Me,5,e))))))}),ur=r(function(n,e){return e.$?n:e.a}),ir=function(n){for(var e=0,r=n.charCodeAt(0),t=43==r||45==r?1:0,o=t;o<n.length;++o){var a=n.charCodeAt(o);if(a<48||57<a)return ee;e=10*e+a-48}return o==t?ee:Zn(45==r?-e:e)},cr=r(function(n,e){return p(e,{n:a(ge,0,a(ur,e.n,ir(n)))})}),fr=r(function(n,e){return p(e,{l:a(cr,n,e.l)})}),sr=r(function(n,e){return p(e,{A:a(ge,0,a(ur,e.A,ir(n)))})}),lr=r(function(n,e){switch(n.$){case 0:return m(e,_e);case 15:return m(function(n){return p(n,{s:4})}(e),_e);case 1:return m(function(n){return p(n,{s:5})}(e),_e);case 2:return m(a(fr,n.a,e),_e);case 3:return m(function(n){var e=n.e.x;if(e.$)return a(oe,"Please address a customer before submitting an offer.",n);var r=e.a;return g(n.l.n,r.af)<1?a(ar,r,n):u(nr,r,n.l,n)}(e),_e);case 4:return m(p(e,{N:s}),_e);case 5:return m(function(n){return p(n,{s:1})}(e),_e);case 6:return m(Ke(e),_e);case 7:return m(function(n){return p(n,{s:0})}(e),_e);case 8:return m(function(n){return!n.w&&a(Ue,n.ab,n.f)?a(Be,"The store closes whilst you are busy cleanings.",n):a(oe,"You clean the store for "+re(n.ab)+" minutes.",a(Me,n.ab,n))}(e),_e);case 9:return m(p(e,{ad:!e.ad}),_e);case 17:return m(function(n){return p(n,{s:3})}(e),_e);case 18:return m(function(n){if(!n.w&&a(Ue,10,n.f))return a(Be,"Whilst you are busy staring at the customer, the store closes, and they leave before you can get a better look.",n);var e=n.e.x;return e.$?a(oe,"Who are you trying to inspeect?",n):a(oe,(r=e.a,"You inspect "+r.a+" for "+re(10)+" minutes. "+r.c+" "+function(){switch(r.b){case 0:return"They seem pretty much destitute.";case 1:return"They seem quite poor.";case 2:return"They seem to be of average wealth.";case 3:return"They seem to be quite well-off";default:return"They appear to be rather wealthy."}}()),a(Me,10,n));var r}(e),_e);case 10:return m(function(n){return p(n,{s:2})}(e),_e);case 11:return m(function(n){if(!n.w&&a(Ue,20,n.f))return a(Be,"Whilst you are busy schmoozing the customer, the store closes, and you are forced to schmoo them out.",n);var e,r,t=n.e.x;return t.$?a(oe,"Who are you trying to schmooze?",n):p(e=a(oe,(o=t.a,g(o.H,3)<0?"You tell "+o.a+" that they have lovely hair. They are impressed and are willing to pay 50% more for the item. This takes "+re(20)+" minutes.":"You tell "+o.a+" that they have lovely hair. They snap at you that you've said that "+re(3)+" times already. They seem annoyed. This takes "+re(20)+" minutes."),a(Me,20,n)),{e:(r=e.e,p(r,{x:a(Ze,Ve,r.x)}))});var o}(e),_e);case 12:return m(a(He,n.a,e),_e);case 13:return m(function(n){return p(n,{l:(e=n.l,p(e,{n:e.T}))});var e}(e),_e);case 14:return m(a(sr,n.a,e),_e);case 16:return m(function(n){return!n.w&&a(Ue,n.A,n.f)?a(Be,"Whilst you are busy sitting on your ass, you lose track of the time, and next thing you know, the store is closed and the customers have left.",n):a(oe,"You sit on your ass for "+re(n.A)+" minutes.",a(Me,n.A,n))}(e),_e);case 19:return m(function(n){return p(n,{s:6})}(e),_e);default:return m(function(n){return a(oe,Pn(Kn),(e=function(n){return p(n,{e:(e=n.e,r=Re(e.y),t=r.b,p(e,{x:Zn(r.a),y:t})),s:5,v:ne,w:0,f:1440*(Le(n.f)+1)+540,X:1440*(Le(n.f)+1)+540+30});var e,r,t}(n),a(oe,"You open your doors on day "+re(Le(e.f))+" of many!",e)));var e}(e),_e)}}),dr=C,hr=function(n){return{$:0,a:n}},vr=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},br=sn,gr=a(br,"border","2px solid black"),mr=un("div"),pr=un("h2"),yr=an,wr={$:4},$r={$:9},kr=un("button"),Ar=un("h3"),xr=un("textarea"),Tr=r(function(n,e){return a(dn,function(n){return/^(on|formAction$)/i.test(n)?"data-"+n:n}(n),function(n){return/^\s*(javascript:|data:text\/html)/i.test(n)?"":n}(e))}),jr=R,Sr=r(function(n,e){return a(ln,n,jr(e))})("readOnly"),Nr=fn,_r=r(function(n,e){return a(Nr,n,{$:0,a:e})}),Cr=function(n){return a(_r,"click",hr(n))},Wr=r(function(n,e){return a(kr,a(qn,a(br,"margin","2px"),n),e)}),Yr=h([a(Ar,s,h([yr("Actions")])),a(mr,s,h([a(Wr,h([Cr({$:19})]),h([yr("Open Store")]))]))]),Hr=h([a(Ar,s,h([yr("Actions")])),a(mr,s,h([a(Wr,h([Cr({$:1})]),h([yr("Sale")])),a(Wr,h([Cr({$:10})]),h([yr("Schmooze")])),a(Wr,h([Cr({$:17})]),h([yr("Inspect")])),a(Wr,h([Cr({$:5})]),h([yr("Kick Out")])),a(Wr,h([Cr({$:7})]),h([yr("Clean")])),a(Wr,h([Cr({$:15})]),h([yr("Wait")]))]))]),Er={$:20},Or={$:8},Lr={$:18},Dr={$:6},qr=function(n){return{$:2,a:n}},Rr={$:13},Fr={$:11},Pr={$:3},zr=function(n){return{$:14,a:n}},Ir={$:16},Br=r(function(n,e){return"Sell "+e.S+" (cost "+re(e.T)+"gp) to "+n.a+" for "+re(e.n)+"gp."}),Mr=r(function(n,e){return a(Wr,h([Cr(qr(re(e.l.n+n)))]),h([yr(y(n>0?"+":"",re(n)))]))}),Jr=r(function(n,e){return a(Wr,h([Cr(zr(re(e.A+n)))]),h([yr(y(n>0?"+":"",re(n)))]))}),Xr=un("br"),Ur=un("input"),Gr=R,Kr=r(function(n,e){return a(ln,n,Gr(e))}),Vr=Kr("max"),Zr=Kr("min"),Qr=Kr("type"),nt=Kr("value"),et=function(n){return m(n,!0)},rt=r(function(n,e){return a(Nr,n,{$:1,a:e})}),tt=N,ot=S,at=a(r(function(n,e){return u(Xn,tt,e,n)}),h(["target","value"]),ot),ut=function(n){return a(rt,"input",a(dr,et,a(dr,n,at)))},it=h([a(Ar,s,h([yr("Customers")])),a(mr,s,h([yr("The store is now closed.")]))]),ct=r(function(n,e){return a(Un,function(e){return a(kr,h([n(e)]),h([yr(e.a)]))},e.D)}),ft=function(n){return a(mr,h([a(br,"float","left"),a(br,"width","50%")]),h([a(mr,h([gr,a(br,"margin-top","5px"),a(br,"margin-bottom","0px"),a(br,"margin-left","5px"),a(br,"margin-right","5px"),a(br,"padding-top","0px"),a(br,"padding-bottom","10px"),a(br,"padding-left","0px"),a(br,"padding-right","0px")]),n)]))},st=function(){return h([a(Ar,s,h([yr("Stock")])),a(mr,s,h([yr("Infinite swords")]))])},lt=r(function(n,e){return r=h(n?[ft(st()),ft(Yr),ft(it),ft(function(n){return h([a(Ar,s,h([yr("Current Action")])),function(){switch(n.s){case 0:case 1:case 2:case 3:case 5:case 4:return a(mr,s,h([yr("The store is now closed.")]));default:return a(mr,s,h([a(Wr,h([Cr(Er)]),h([yr("Skip until tomorrow and open store at "+re(9)+" o Clock.")]))]))}}()])}(e))]:[ft(st()),ft(Hr),ft(function(n){return h([a(Ar,s,h([yr("Customers")])),a(mr,s,a(ct,function(n){return Cr({$:12,a:n})},n.e)),a(Xr,s,s),a(mr,s,h([yr("You are speaking to: "+(e=n.e.x,(e.$?"No-one":e.a.a)+"."))]))]);var e}(e)),ft(function(n){return h([a(Ar,s,h([yr("Current Action")])),function(){switch(n.s){case 0:return a(mr,s,h([a(Wr,h([Cr(Or)]),h([yr("Clean Store")]))]));case 1:var e=n.e.x;if(1===e.$)return a(mr,s,h([yr("Select a customer to address.")]));var r=e.a;return a(mr,s,h([a(Wr,h([Cr(Dr)]),h([yr("Kick out "+r.a)]))]));case 2:var t=n.e.x;return 1===t.$?a(mr,s,h([yr("Select a customer to address.")])):(r=t.a,a(mr,s,h([a(Wr,h([Cr(Fr)]),h([yr("Schmooze "+r.a)]))])));case 3:var o=n.e.x;return 1===o.$?a(mr,s,h([yr("Select a customer to address.")])):(r=o.a,a(mr,s,h([a(Wr,h([Cr(Lr)]),h([yr("Inspect "+r.a)]))])));case 5:var u=n.e.x;return 1===u.$?a(mr,s,h([yr("Select a customer to address.")])):(r=u.a,a(mr,s,h([a(mr,h([a(br,"margin-bottom","5px")]),h([a(mr,s,h([a(Wr,h([Cr(Rr)]),h([yr("Reset")])),a(Mr,-100,n),a(Mr,-10,n),a(Ur,h([a(Tr,"aria-label","Price in gold"),a(br,"margin","2px"),Qr("number"),Zr("0"),Vr("50000"),nt(re(n.l.n)),ut(qr)]),s),a(Mr,10,n),a(Mr,100,n)])),a(Xr,s,s),a(mr,s,s),a(Wr,h([Cr(Pr)]),h([yr(a(Br,r,n.l))]))]))])));case 4:return a(mr,s,h([a(Wr,h([Cr(zr(re(0)))]),h([yr("Reset")])),a(Jr,-60,n),a(Jr,-10,n),a(Ur,h([a(Tr,"aria-label","Time to wait"),a(br,"margin","2px"),Qr("number"),Zr("0"),Vr("1440"),nt(re(n.A)),ut(zr)]),s),a(Jr,10,n),a(Jr,60,n),a(mr,s,s),a(Wr,h([Cr(Ir)]),h([yr("Wait for "+re(n.A)+" minutes.")]))]));default:return a(mr,s,h([yr("The store must be closed for you to be able to open it.")]))}}()])}(e))]),a(mr,h([a(br,"width","100%"),a(br,"clear","both"),a(br,"display","table")]),h([a(mr,h([a(br,"margin-top","5px"),a(br,"margin-bottom","10px"),a(br,"margin-left","5px"),a(br,"margin-right","5px")]),r)]));var r}),dt=un("h1"),ht=P,vt=ht(0),bt=I,gt=r(function(n,e){return a(bt,function(e){return ht(n(e))},e)}),mt=t(function(n,e,r){return a(bt,function(e){return a(bt,function(r){return ht(a(n,e,r))},r)},e)}),pt=Q,yt=r(function(n,e){var r=e;return function(n){return z(function(e){e(P(M(n)))})}(a(bt,pt(n),r))});K.Task={b:vt,c:t(function(n,e){return a(gt,function(){return 0},(r=a(Un,yt(n),e),u(Xn,mt(qn),ht(s),r)));var r}),d:t(function(){return ht(0)}),e:r(function(n,e){return a(gt,n,e)}),f:void 0};var wt,$t=_,kt=j;wt={Main:{init:On({bh:function(n){return m(a(oe,Pn(Kn),{ab:10,N:s,e:Qn,ad:!0,l:{S:"sword",T:20,n:20},aj:0,s:5,v:ne,w:0,f:540,X:570,A:0,_:n._}),_e)},bn:r(function(n){return n})(nn(s)),bp:lr,br:function(n){return a(mr,s,h([a(dt,s,h([yr("Trading Post")])),yr("https://mark-chimes.github.io/trading-post/"),a(pr,s,h([yr("Debug")])),yr("Time of next customer "+te(n.X)),a(mr,s,s),yr("minutesSinceZero: "+re(n.f)),a(mr,s,s),yr("Time: "+te(n.f)),a(mr,s,s),yr("Day: "+re(Le(n.f))),(e=function(n){return h([a(pr,s,h([yr("Store")])),a(mr,s,h([yr("Time: "+te(n.f))])),a(mr,s,h([yr("Your gold: "+re(n.aj)+"gp")]))])}(n),a(mr,h([a(br,"width","100%")]),h([a(mr,h([gr,a(br,"margin-bottom","5px"),a(br,"margin-left","10px"),a(br,"margin-right","10px"),a(br,"padding-bottom","10px")]),e)]))),a(lt,n.w,n),function(n){return a(mr,h([a(br,"width","100%"),a(br,"clear","both"),a(br,"display","table")]),h([a(mr,h([gr,a(br,"margin-top","10px"),a(br,"margin-bottom","5px"),a(br,"margin-left","10px"),a(br,"margin-right","10px"),a(br,"padding-bottom","10px")]),n)]))}(function(n){return h([a(Ar,s,h([yr("The story thus far: ")])),a(kr,h([Cr(wr)]),h([yr("Clear Story")])),a(kr,h([Cr($r)]),h([yr("Reverse Story")])),a(mr,s,s),a(xr,h([a(Tr,"aria-live","assertive"),Sr(!0),(r=n._/10|0,a(dn,"cols",re(r))),a(dn,"rows",re(15))]),(n.ad?Mn:function(n){return n})(a(Un,function(n){return yr(n+"\n")},(e=n.N,a(Un,function(n){return a(Ne,"\n",n)},e)))))]);var e,r}(n))]));var e}})(a($t,function(n){return hr({_:n})},a(tt,"windowWidth",kt)))(0)}},n.Elm?function n(e,r){for(var t in r)t in e?"init"==t?v(6):n(e[t],r[t]):e[t]=r[t]}(n.Elm,wt):n.Elm=wt}(this)},function(n,e,r){r(3),n.exports=r(11)},,,,,,,,function(){},function(n,e,r){"use strict";r.r(e),r(10);var t=r(1),o=!("localhost"!==window.location.hostname&&"[::1]"!==window.location.hostname&&!window.location.hostname.match(/^127(?:\.(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3}$/));function a(n,e){navigator.serviceWorker.register(n).then(function(n){n.onupdatefound=function(){var r=n.installing;null!=r&&(r.onstatechange=function(){"installed"===r.state&&(navigator.serviceWorker.controller?(console.log("New content is available and will be used when all tabs for this page are closed. See https://bit.ly/CRA-PWA."),e&&e.onUpdate&&e.onUpdate(n)):(console.log("Content is cached for offline use."),e&&e.onSuccess&&e.onSuccess(n)))})}}).catch(function(n){console.error("Error during service worker registration:",n)})}t.Elm.Main.init({node:document.getElementById("root"),flags:{windowWidth:window.innerWidth}}),function(){if("serviceWorker"in navigator){if(new URL("/trading-post",window.location.href).origin!==window.location.origin)return;window.addEventListener("load",function(){var n="".concat("/trading-post","/service-worker.js");o?(function(n){fetch(n).then(function(e){var r=e.headers.get("content-type");404===e.status||null!=r&&-1===r.indexOf("javascript")?navigator.serviceWorker.ready.then(function(n){n.unregister().then(function(){window.location.reload()})}):a(n,void 0)}).catch(function(){console.log("No internet connection found. App is running in offline mode.")})}(n),navigator.serviceWorker.ready.then(function(){console.log("This web app is being served cache-first by a service worker. To learn more, visit https://bit.ly/CRA-PWA")})):a(n,void 0)})}}()}],[[2,1,2]]]);
//# sourceMappingURL=main.07fa7243.chunk.js.map