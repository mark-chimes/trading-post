(window.webpackJsonp=window.webpackJsonp||[]).push([[0],[,function(){!function(n){"use strict";function e(n,e,r){return r.a=n,r.f=e,r}function r(n){return e(2,n,function(e){return function(r){return n(e,r)}})}function t(n){return e(3,n,function(e){return function(r){return function(t){return n(e,r,t)}}})}function o(n){return e(4,n,function(e){return function(r){return function(t){return function(o){return n(e,r,t,o)}}}})}function a(n,e,r){return 2===n.a?n.f(e,r):n(e)(r)}function u(n,e,r,t){return 3===n.a?n.f(e,r,t):n(e)(r)(t)}function i(n,e,r,t,o){return 4===n.a?n.f(e,r,t,o):n(e)(r)(t)(o)}var c=t(function(n,e,r){for(var t=Array(n),o=0;o<n;o++)t[o]=r(e+o);return t}),f=r(function(n,e){for(var r=Array(n),t=0;t<n&&e.b;t++)r[t]=e.a,e=e.b;return r.length=t,p(r,e)}),s={$:0};function l(n,e){return{$:1,a:n,b:e}}var d=r(l);function h(n){for(var e=s,r=n.length;r--;)e=l(n[r],e);return e}function v(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}function b(n,e,r,t){if(r>100)return t.push(p(n,e)),!0;if(n===e)return!0;if("object"!==typeof n||null===n||null===e)return"function"===typeof n&&v(5),!1;for(var o in n.$<0&&(n=Rn(n),e=Rn(e)),n)if(!b(n[o],e[o],r+1,t))return!1;return!0}function g(n,e,r){if("object"!==typeof n)return n===e?0:n<e?-1:1;if("undefined"===typeof n.$)return(r=g(n.a,e.a))?r:(r=g(n.b,e.b))?r:g(n.c,e.c);for(;n.b&&e.b&&!(r=g(n.a,e.a));n=n.b,e=e.b);return r||(n.b?1:e.b?-1:0)}function p(n,e){return{a:n,b:e}}function m(n,e){var r={};for(var t in n)r[t]=n[t];for(var t in e)r[t]=e[t];return r}function y(n,e){if("string"===typeof n)return n+e;if(!n.b)return e;var r=l(n.a,e);n=n.b;for(var t=r;n.b;n=n.b)t=t.b=l(n.a,e);return r}var w=r(function(n,e){var r=e%n;return 0===n?v(11):r>0&&n<0||r<0&&n>0?r+n:r}),$=Math.ceil,k=Math.floor,A=Math.log,x=r(function(n,e){return e.join(n)});function j(n){return{$:2,b:n}}var T=j(function(n){return"number"!==typeof n?B("an INT",n):-2147483647<n&&n<2147483647&&(0|n)===n?xe(n):!isFinite(n)||n%1?B("an INT",n):xe(n)}),S=(j(function(n){return"boolean"===typeof n?xe(n):B("a BOOL",n)}),j(function(n){return"number"===typeof n?xe(n):B("a FLOAT",n)}),j(function(n){return xe(H(n))}),j(function(n){return"string"===typeof n?xe(n):n instanceof String?xe(n+""):B("a STRING",n)})),E=r(function(n,e){return{$:6,d:n,b:e}});var _=r(function(n,e){return{$:10,b:e,h:n}}),C=r(function(n,e){return function(n,e){return{$:9,f:n,g:e}}(n,[e])}),N=r(function(n,e){return W(n,R(e))});function W(n,e){switch(n.$){case 2:return n.b(e);case 5:return null===e?xe(n.c):B("null",e);case 3:return z(e)?Y(n.b,e,h):B("a LIST",e);case 4:return z(e)?Y(n.b,e,O):B("an ARRAY",e);case 6:var r=n.d;if("object"!==typeof e||null===e||!(r in e))return B("an OBJECT with a field named `"+r+"`",e);var t=W(n.b,e[r]);return ue(t)?t:Ae(a(Te,r,t.a));case 7:var o=n.e;return z(e)?o<e.length?(t=W(n.b,e[o]),ue(t)?t:Ae(a(Se,o,t.a))):B("a LONGER array. Need index "+o+" but only see "+e.length+" entries",e):B("an ARRAY",e);case 8:if("object"!==typeof e||null===e||z(e))return B("an OBJECT",e);var u=s;for(var i in e)if(e.hasOwnProperty(i)){if(t=W(n.b,e[i]),!ue(t))return Ae(a(Te,i,t.a));u=l(p(i,t.a),u)}return xe(Dn(u));case 9:for(var c=n.f,f=n.g,d=0;d<f.length;d++){if(t=W(f[d],e),!ue(t))return t;c=c(t.a)}return xe(c);case 10:return t=W(n.b,e),ue(t)?W(n.h(t.a),e):t;case 11:for(var v=s,b=n.g;b.b;b=b.b){if(t=W(b.a,e),ue(t))return t;v=l(t.a,v)}return Ae(Ee(Dn(v)));case 1:return Ae(a(je,n.a,H(e)));case 0:return xe(n.a)}}function Y(n,e,r){for(var t=e.length,o=Array(t),u=0;u<t;u++){var i=W(n,e[u]);if(!ue(i))return Ae(a(Se,u,i.a));o[u]=i.a}return xe(r(o))}function z(n){return Array.isArray(n)||"function"===typeof FileList&&n instanceof FileList}function O(n){return a(ke,n.length,function(e){return n[e]})}function B(n,e){return Ae(a(je,"Expecting "+n,H(e)))}function L(n,e){if(n===e)return!0;if(n.$!==e.$)return!1;switch(n.$){case 0:case 1:return n.a===e.a;case 2:return n.b===e.b;case 5:return n.c===e.c;case 3:case 4:case 8:return L(n.b,e.b);case 6:return n.d===e.d&&L(n.b,e.b);case 7:return n.e===e.e&&L(n.b,e.b);case 9:return n.f===e.f&&q(n.g,e.g);case 10:return n.h===e.h&&L(n.b,e.b);case 11:return q(n.g,e.g)}}function q(n,e){var r=n.length;if(r!==e.length)return!1;for(var t=0;t<r;t++)if(!L(n[t],e[t]))return!1;return!0}function H(n){return n}function R(n){return n}function M(n){return{$:0,a:n}}function P(n){return{$:2,b:n,c:null}}H(null);var F=r(function(n,e){return{$:3,b:n,d:e}}),I=0;function D(n){var e={$:0,e:I++,f:n,g:null,h:[]};return X(e),e}var J=!1,Q=[];function X(n){if(Q.push(n),!J){for(J=!0;n=Q.shift();)G(n);J=!1}}function G(n){for(;n.f;){var e=n.f.$;if(0===e||1===e){for(;n.g&&n.g.$!==e;)n.g=n.g.i;if(!n.g)return;n.f=n.g.b(n.f.a),n.g=n.g.i}else{if(2===e)return void(n.f.c=n.f.b(function(e){n.f=e,X(n)}));if(5===e){if(0===n.h.length)return;n.f=n.f.b(n.h.shift())}else n.g={$:3===e?0:1,b:n.f.b,i:n.g},n.f=n.f.d}}}var U={};function K(n,e){var r={g:e,h:void 0},t=n.c,o=n.d,c=n.e,f=n.f;return r.h=D(a(F,function n(e){return a(F,n,{$:5,b:function(n){var a=n.a;return 0===n.$?u(o,r,a,e):c&&f?i(t,r,a.i,a.j,e):u(t,r,c?a.i:a.j,e)}})},n.b))}var V,Z=r(function(n,e){return P(function(r){n.g(e),r(M(0))})});function nn(n){return{$:2,m:n}}function en(n,e,r){var t,o={};for(var a in rn(!0,e,o,null),rn(!1,r,o,null),n)(t=n[a]).h.push({$:"fx",a:o[a]||{i:s,j:s}}),X(t)}function rn(n,e,r,t){switch(e.$){case 1:var o=e.k,u=function(n,r,t){return a(n?U[r].e:U[r].f,function(n){for(var e=t;e;e=e.q)n=e.p(n);return n},e.l)}(n,o,t);return void(r[o]=function(n,e,r){return r=r||{i:s,j:s},n?r.i=l(e,r.i):r.j=l(e,r.j),r}(n,u,r[o]));case 2:for(var i=e.m;i.b;i=i.b)rn(n,i.a,r,t);return;case 3:return void rn(n,e.o,r,{p:e.n,q:t})}}var tn="undefined"!==typeof document?document:{};function on(n,e){n.appendChild(e)}function an(n){return{$:0,a:n}}var un=r(function(n,e){return r(function(r,t){for(var o=[],a=0;t.b;t=t.b){var u=t.a;a+=u.b||0,o.push(u)}return a+=o.length,{$:1,c:e,d:hn(r),e:o,f:n,b:a}})})(void 0);r(function(n,e){return r(function(r,t){for(var o=[],a=0;t.b;t=t.b){var u=t.a;a+=u.b.b||0,o.push(u)}return a+=o.length,{$:2,c:e,d:hn(r),e:o,f:n,b:a}})})(void 0);var cn,fn=r(function(n,e){return{$:"a0",n:n,o:e}}),sn=r(function(n,e){return{$:"a1",n:n,o:e}}),ln=r(function(n,e){return{$:"a2",n:n,o:e}}),dn=r(function(n,e){return{$:"a3",n:n,o:e}});function hn(n){for(var e={};n.b;n=n.b){var r=n.a,t=r.$,o=r.n,a=r.o;if("a2"!==t){var u=e[t]||(e[t]={});"a3"===t&&"class"===o?vn(u,o,a):u[o]=a}else"className"===o?vn(e,o,R(a)):e[o]=R(a)}return e}function vn(n,e,r){var t=n[e];n[e]=t?t+" "+r:r}function bn(n,e){var r=n.$;if(5===r)return bn(n.k||(n.k=n.m()),e);if(0===r)return tn.createTextNode(n.a);if(4===r){for(var t=n.k,o=n.j;4===t.$;)"object"!==typeof o?o=[o,t.j]:o.push(t.j),t=t.k;var a={j:o,p:e};return(u=bn(t,a)).elm_event_node_ref=a,u}if(3===r)return gn(u=n.h(n.g),e,n.d),u;var u=n.f?tn.createElementNS(n.f,n.c):tn.createElement(n.c);V&&"a"==n.c&&u.addEventListener("click",V(u)),gn(u,e,n.d);for(var i=n.e,c=0;c<i.length;c++)on(u,bn(1===r?i[c]:i[c].b,e));return u}function gn(n,e,r){for(var t in r){var o=r[t];"a1"===t?pn(n,o):"a0"===t?wn(n,e,o):"a3"===t?mn(n,o):"a4"===t?yn(n,o):("value"!==t&&"checked"!==t||n[t]!==o)&&(n[t]=o)}}function pn(n,e){var r=n.style;for(var t in e)r[t]=e[t]}function mn(n,e){for(var r in e){var t=e[r];"undefined"!==typeof t?n.setAttribute(r,t):n.removeAttribute(r)}}function yn(n,e){for(var r in e){var t=e[r],o=t.f,a=t.o;"undefined"!==typeof a?n.setAttributeNS(o,r,a):n.removeAttributeNS(o,r)}}function wn(n,e,r){var t=n.elmFs||(n.elmFs={});for(var o in r){var a=r[o],u=t[o];if(a){if(u){if(u.q.$===a.$){u.q=a;continue}n.removeEventListener(o,u)}u=$n(e,a),n.addEventListener(o,u,cn&&{passive:yr(a)<2}),t[o]=u}else n.removeEventListener(o,u),t[o]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){cn=!0}}))}catch(n){}function $n(n,e){function r(e){var t=r.q,o=W(t.a,e);if(ue(o)){for(var a,u=yr(t),i=o.a,c=u?u<3?i.a:i.G:i,f=1==u?i.b:3==u&&i.aC,s=(f&&e.stopPropagation(),(2==u?i.b:3==u&&i.aA)&&e.preventDefault(),n);a=s.j;){if("function"==typeof a)c=a(c);else for(var l=a.length;l--;)c=a[l](c);s=s.p}s(c,f)}}return r.q=e,r}function kn(n,e){return n.$==e.$&&L(n.a,e.a)}function An(n,e,r,t){var o={$:e,r:r,s:t,t:void 0,u:void 0};return n.push(o),o}function xn(n,e,r,t){if(n!==e){var o=n.$,a=e.$;if(o!==a){if(1!==o||2!==a)return void An(r,0,t,e);e=function(n){for(var e=n.e,r=e.length,t=Array(r),o=0;o<r;o++)t[o]=e[o].b;return{$:1,c:n.c,d:n.d,e:t,f:n.f,b:n.b}}(e),a=1}switch(a){case 5:for(var u=n.l,i=e.l,c=u.length,f=c===i.length;f&&c--;)f=u[c]===i[c];if(f)return void(e.k=n.k);e.k=e.m();var s=[];return xn(n.k,e.k,s,0),void(s.length>0&&An(r,1,t,s));case 4:for(var l=n.j,d=e.j,h=!1,v=n.k;4===v.$;)h=!0,"object"!==typeof l?l=[l,v.j]:l.push(v.j),v=v.k;for(var b=e.k;4===b.$;)h=!0,"object"!==typeof d?d=[d,b.j]:d.push(b.j),b=b.k;return h&&l.length!==d.length?void An(r,0,t,e):((h?function(n,e){for(var r=0;r<n.length;r++)if(n[r]!==e[r])return!1;return!0}(l,d):l===d)||An(r,2,t,d),void xn(v,b,r,t+1));case 0:return void(n.a!==e.a&&An(r,3,t,e.a));case 1:return void jn(n,e,r,t,Sn);case 2:return void jn(n,e,r,t,En);case 3:if(n.h!==e.h)return void An(r,0,t,e);var g=Tn(n.d,e.d);g&&An(r,4,t,g);var p=e.i(n.g,e.g);return void(p&&An(r,5,t,p))}}}function jn(n,e,r,t,o){if(n.c===e.c&&n.f===e.f){var a=Tn(n.d,e.d);a&&An(r,4,t,a),o(n,e,r,t)}else An(r,0,t,e)}function Tn(n,e,r){var t;for(var o in n)if("a1"!==o&&"a0"!==o&&"a3"!==o&&"a4"!==o)if(o in e){var a=n[o],u=e[o];a===u&&"value"!==o&&"checked"!==o||"a0"===r&&kn(a,u)||((t=t||{})[o]=u)}else(t=t||{})[o]=r?"a1"===r?"":"a0"===r||"a3"===r?void 0:{f:n[o].f,o:void 0}:"string"===typeof n[o]?"":null;else{var i=Tn(n[o],e[o]||{},o);i&&((t=t||{})[o]=i)}for(var c in e)c in n||((t=t||{})[c]=e[c]);return t}function Sn(n,e,r,t){var o=n.e,a=e.e,u=o.length,i=a.length;u>i?An(r,6,t,{v:i,i:u-i}):u<i&&An(r,7,t,{v:u,e:a});for(var c=u<i?u:i,f=0;f<c;f++){var s=o[f];xn(s,a[f],r,++t),t+=s.b||0}}function En(n,e,r,t){for(var o=[],a={},u=[],i=n.e,c=e.e,f=i.length,s=c.length,l=0,d=0,h=t;l<f&&d<s;){var v=(T=i[l]).a,b=(S=c[d]).a,g=T.b,p=S.b,m=void 0,y=void 0;if(v!==b){var w=i[l+1],$=c[d+1];if(w){var k=w.a,A=w.b;y=b===k}if($){var x=$.a,j=$.b;m=v===x}if(m&&y)xn(g,j,o,++h),Cn(a,o,v,p,d,u),h+=g.b||0,Nn(a,o,v,A,++h),h+=A.b||0,l+=2,d+=2;else if(m)h++,Cn(a,o,b,p,d,u),xn(g,j,o,h),h+=g.b||0,l+=1,d+=2;else if(y)Nn(a,o,v,g,++h),h+=g.b||0,xn(A,p,o,++h),h+=A.b||0,l+=2,d+=1;else{if(!w||k!==x)break;Nn(a,o,v,g,++h),Cn(a,o,b,p,d,u),h+=g.b||0,xn(A,j,o,++h),h+=A.b||0,l+=2,d+=2}}else xn(g,p,o,++h),h+=g.b||0,l++,d++}for(;l<f;){var T;Nn(a,o,(T=i[l]).a,g=T.b,++h),h+=g.b||0,l++}for(;d<s;){var S,E=E||[];Cn(a,o,(S=c[d]).a,S.b,void 0,E),d++}(o.length>0||u.length>0||E)&&An(r,8,t,{w:o,x:u,y:E})}var _n="_elmW6BL";function Cn(n,e,r,t,o,a){var u=n[r];if(!u)return a.push({r:o,A:u={c:0,z:t,r:o,s:void 0}}),void(n[r]=u);if(1===u.c){a.push({r:o,A:u}),u.c=2;var i=[];return xn(u.z,t,i,u.r),u.r=o,void(u.s.s={w:i,A:u})}Cn(n,e,r+_n,t,o,a)}function Nn(n,e,r,t,o){var a=n[r];if(a){if(0===a.c){a.c=2;var u=[];return xn(t,a.z,u,o),void An(e,9,o,{w:u,A:a})}Nn(n,e,r+_n,t,o)}else{var i=An(e,9,o,void 0);n[r]={c:1,z:t,r:o,s:i}}}function Wn(n,e,r,t){return 0===r.length?n:(function n(e,r,t,o){!function e(r,t,o,a,u,i,c){for(var f=o[a],s=f.r;s===u;){var l=f.$;if(1===l)n(r,t.k,f.s,c);else if(8===l)f.t=r,f.u=c,(d=f.s.w).length>0&&e(r,t,d,0,u,i,c);else if(9===l){f.t=r,f.u=c;var d,h=f.s;h&&(h.A.s=r,(d=h.w).length>0&&e(r,t,d,0,u,i,c))}else f.t=r,f.u=c;if(!(f=o[++a])||(s=f.r)>i)return a}var v=t.$;if(4===v){for(var b=t.k;4===b.$;)b=b.k;return e(r,b,o,a,u+1,i,r.elm_event_node_ref)}for(var g=t.e,p=r.childNodes,m=0;m<g.length;m++){u++;var y=1===v?g[m]:g[m].b,w=u+(y.b||0);if(u<=s&&s<=w&&(!(f=o[a=e(p[m],y,o,a,u,w,c)])||(s=f.r)>i))return a;u=w}return a}(e,r,t,0,0,r.b,o)}(n,e,r,t),Yn(n,r))}function Yn(n,e){for(var r=0;r<e.length;r++){var t=e[r],o=t.t,a=zn(o,t);o===n&&(n=a)}return n}function zn(n,e){switch(e.$){case 0:return function(n){var r=n.parentNode,t=bn(e.s,e.u);return t.elm_event_node_ref||(t.elm_event_node_ref=n.elm_event_node_ref),r&&t!==n&&r.replaceChild(t,n),t}(n);case 4:return gn(n,e.u,e.s),n;case 3:return n.replaceData(0,n.length,e.s),n;case 1:return Yn(n,e.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=e.s:n.elm_event_node_ref={j:e.s,p:e.u},n;case 6:for(var r=e.s,t=0;t<r.i;t++)n.removeChild(n.childNodes[r.v]);return n;case 7:for(var o=(r=e.s).e,a=n.childNodes[t=r.v];t<o.length;t++)n.insertBefore(bn(o[t],e.u),a);return n;case 9:if(!(r=e.s))return n.parentNode.removeChild(n),n;var u=r.A;return"undefined"!==typeof u.r&&n.parentNode.removeChild(n),u.s=Yn(n,r.w),n;case 8:return function(n,e){var r=e.s,t=function(n,e){if(n){for(var r=tn.createDocumentFragment(),t=0;t<n.length;t++){var o=n[t].A;on(r,2===o.c?o.s:bn(o.z,e.u))}return r}}(r.y,e);n=Yn(n,r.w);for(var o=r.x,a=0;a<o.length;a++){var u=o[a],i=u.A,c=2===i.c?i.s:bn(i.z,e.u);n.insertBefore(c,n.childNodes[u.r])}return t&&on(n,t),n}(n,e);case 5:return e.s(n);default:v(10)}}var On=o(function(n,e,r,t){return function(n,e,r,t,o,u){var i=a(N,n,H(e?e.flags:void 0));ue(i)||v(2);var c={},f=(i=r(i.a)).a,s=u(d,f),l=function(n,e){var r;for(var t in U){var o=U[t];o.a&&((r=r||{})[t]=o.a(t,e)),n[t]=K(o,e)}return r}(c,d);function d(n,e){s(f=(i=a(t,n,f)).a,e),en(c,i.b,o(f))}return en(c,i.b,o(f)),l?{ports:l}:{}}(e,t,n.bg,n.bp,n.bn,function(e,r){var o=n.br,i=t.node,c=function n(e){if(3===e.nodeType)return an(e.textContent);if(1!==e.nodeType)return an("");for(var r=s,t=e.attributes,o=t.length;o--;){var i=t[o];r=l(a(dn,i.name,i.value),r)}var c=e.tagName.toLowerCase(),f=s,d=e.childNodes;for(o=d.length;o--;)f=l(n(d[o]),f);return u(un,c,r,f)}(i);return function(n,e){e(n);var r=0;function t(){r=1===r?0:(Bn(t),e(n),1)}return function(o,a){n=o,a?(e(n),2===r&&(r=1)):(0===r&&Bn(t),r=2)}}(r,function(n){var r=o(n),t=function(n,e){var r=[];return xn(n,e,r,0),r}(c,r);i=Wn(i,c,t,e),c=r})})}),Bn=("undefined"!==typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!==typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)});"undefined"!==typeof document&&document,"undefined"!==typeof window&&window;var Ln,qn=d,Hn=t(function(n,e,r){for(;;){if(-2===r.$)return e;var t=r.d,o=n,a=u(n,r.b,r.c,u(Hn,n,e,r.e));n=o,e=a,r=t}}),Rn=function(n){return u(Hn,t(function(n,e,r){return a(qn,p(n,e),r)}),s,n)},Mn=function(n){return"You begin speaking to a customer named "+n.a+". "+n.d},Pn=function(n){return{c:n.c,d:n.d,a:n.a,M:0,b:n.b}},Fn=h([{c:"She has wonderful hair!",d:"She browses your product line.",a:"Carol Cooper-Iardlynoer",b:2},{c:"Just a teenage dirtbag baby.",d:"He keeps his hands in his pockets.",a:"Dennis Demacia",b:1},{c:"An iron maiden.",d:"She strides up to your counter confidently in full plate.",a:"Erica Earful",b:4},{c:"Frankly, my dear, he doesn't give a damn.",d:"He seems like he couldn't care less.",a:"Frank Mann-Free",b:2},{c:"Her clothes are expensive, but well-worn - a wealthy woman who has fallen on harder times.",d:"A prim and proper older lady.",a:"Gertrude Ganderstudies",b:2},{c:"He growls when he speaks, and strange jewellery flashes from under his cloak. He keeps muttering about broken swords and half-things.",d:"A dark and mysterious cloaked figure.",a:"Harold Harbinger",b:4},{c:"On closer inspection, the ears appear to be a form of costume jewellery.",d:"A middle-aged woman with surprisingly pointy ears.",a:"Ingrid Isntmael",b:3},{c:"It is quickly obvious the confidence is just a ruse to hide deep-seated insecurities.",d:"A an attractive, confident man who'll flirt with anyone in the store.",a:"Jerome Jackinthebox",b:1},{c:"She speaks in dark and gravelly voice that tends to make people uncomfortable.",d:"Why is it so chilly in here all of a sudden?",a:"Kyla Killthemall",b:3},{c:"His clothes are tighter than they need to be.",d:"An athletic figure.",a:"Liam Lemonmeringue",b:3},{c:"Looking to make an impression.",d:"Very stylish and fashionable",a:"Marion Mansion",b:4},{c:"A friendly chap who manages to maintain a positive attitude despite his unfortunate conditions.",d:"A beggar has made his way into your store.",a:"Noddy Noboddy",b:0},{c:"She speaks at length about her grandchildren in the East.",d:"A kindly looking old lady.",a:"Olivia Oldbutgold",b:3},{c:"He has numerous bags of colourful spices arrayed inside his jacket.",d:"A fragrant and spicy smell wafts through the store.",a:"Patrick Pleasepassthepepper",b:2},{c:"Dressed thick and warm, as if expecting a cold winter.",d:"A dappled and eye-catching array of clothing.",a:"Quinette Qualityquilt",b:3},{c:"Actually quite friendly and well-mannered, just has a hunch back and a lazy eye.",d:"Creeping in with back hunched, eying the racks suspiciously.",a:"Rawry Ragna-Rock",b:2},{c:"She keeps offering you to try some of her quadruple-ginger cookies.",d:"A plump middle-aged woman.",a:"Samantha Saltoftheearth",b:1},{c:"His responses are brief and his mind appears to be elsewhere.",d:"A quiet gentelman standing off on his own.",a:"Toby Tell-Noboddy",b:1},{c:"She gives the impression that she always gets what she wants, sooner or later...",d:"A large and confident lady dressed in enormous robes of purple and strutting in with purpose.",a:"Ursula Ur",b:4},{c:"He tells all about his hobby as a beekeeper and offers to let you try some honey.",d:"A burn mark scars his face.",a:"Vaughn Vatofacid",b:2},{c:"She's wearing quite a lot of makeup, and flirts with everyone in store.",d:"An attractive young woman.",a:"Wendy Mann-Woo",b:2},{c:"He seems incredibly eager to kill goblins.",d:"A big, burly man covered in tattoos",a:"Xavier Xtraspicy",b:2},{c:"Her voice booms loudly, and yet remains pleasant to the ears.",d:"Her muscles ripple as she walks.",a:"Yennefer Yodalayheehoo",b:3},{c:"He speaks slowly and often seems to be a bit lost in his own world.",d:"An attractive young man with a scraggly beard and reddish eyes.",a:"Zander Zoinkies",b:1}]),In=t(function(n,e,r){for(;;){if(!r.b)return e;var t=r.b,o=n,u=a(n,r.a,e);n=o,e=u,r=t}}),Dn=function(n){return u(In,qn,s,n)},Jn=o(function(n,e,r,t){if(t.b){var o=t.a,c=t.b;if(c.b){var f=c.a,s=c.b;if(s.b){var l=s.a,d=s.b;if(d.b){var h=d.b;return a(n,o,a(n,f,a(n,l,a(n,d.a,r>500?u(In,n,e,Dn(h)):i(Jn,n,e,r+1,h)))))}return a(n,o,a(n,f,a(n,l,e)))}return a(n,o,a(n,f,e))}return a(n,o,e)}return e}),Qn=t(function(n,e,r){return i(Jn,n,e,0,r)}),Xn=r(function(n,e){return u(Qn,r(function(e,r){return a(qn,n(e),r)}),s,e)}),Gn=a(Xn,Pn,Fn),Un=Pn({c:"The smile was endearing at first, but it starts to get creepy after awhile.",d:"She greets you with a smile.",a:"Abby Aubergine",b:1}),Kn=h([Pn({c:"Sleazy looking guy. You'd be willing to sell to him, but probably shouldn't trust anything he sold you.",d:"He eyes your store shiftily.",a:"Bob Bucket",b:1})]),Vn=function(n){return{$:0,a:n}},Zn={y:Vn(Un),z:Gn,aw:2,aN:6,E:Kn},ne={P:0,Q:0,ac:0,aj:0,an:0},ee={T:"sword",bh:20},re={$:1},te=function(n){return n+""},oe=function(n){var e=function(n){return p((n/60|0)%24,n%60)}(n),r=e.a,t=e.b;return(r<10?"0":"")+te(r)+":"+(t<10?"0":"")+te(t)},ae=r(function(n,e){return m(e,{O:y(e.O,h([h([oe(e.f),n,""])]))})}),ue=function(n){return!n.$},ie=o(function(n,e,r,t){return{$:0,a:n,b:e,c:r,d:t}}),ce=$,fe=r(function(n,e){return A(e)/A(n)}),se=ce(a(fe,2,32)),le=[],de=i(ie,0,se,le,le),he=f,ve=r(function(n,e){for(;;){var r=a(he,32,n),t=r.b,o=a(qn,{$:0,a:r.a},e);if(!t.b)return Dn(o);n=t,e=o}}),be=r(function(n,e){for(;;){var r=ce(e/32);if(1===r)return a(he,32,n).a;n=a(ve,n,s),e=r}}),ge=k,pe=r(function(n,e){return g(n,e)>0?n:e}),me=function(n){return n.length},ye=r(function(n,e){if(e.g){var r=32*e.g,t=ge(a(fe,32,r-1)),o=n?Dn(e.k):e.k,u=a(be,o,e.g);return i(ie,me(e.j)+r,a(pe,5,t*se),u,e.j)}return i(ie,me(e.j),se,le,e.j)}),we=c,$e=e(5,Ln=function(n,e,r,t,o){for(;;){if(e<0)return a(ye,!1,{k:t,g:r/32|0,j:o});var i={$:1,a:u(we,32,e,n)};n=n,e-=32,r=r,t=a(qn,i,t),o=o}},function(n){return function(e){return function(r){return function(t){return function(o){return Ln(n,e,r,t,o)}}}}}),ke=r(function(n,e){if(n>0){var r=n%32;return t=$e,o=e,a=n-r-32,i=n,c=s,f=u(we,r,n-r,e),5===t.a?t.f(o,a,i,c,f):t(o)(a)(i)(c)(f)}var t,o,a,i,c,f;return de}),Ae=function(n){return{$:1,a:n}},xe=function(n){return{$:0,a:n}},je=r(function(n,e){return{$:3,a:n,b:e}}),Te=r(function(n,e){return{$:0,a:n,b:e}}),Se=r(function(n,e){return{$:1,a:n,b:e}}),Ee=function(n){return{$:2,a:n}},_e=r(function(n,e){return a(x,n,function(n){for(var e=[];n.b;n=n.b)e.push(n.a);return e}(e))}),Ce=nn(s),Ne=r(function(n,e){return u(Qn,r(function(e,r){return n(e)?a(qn,e,r):r}),s,e)}),We=t(function(n,e,r){return y(a(Ne,function(n){return!function(n,e){for(var r,t=[],o=b(n,e,0,t);o&&(r=t.pop());o=b(r.a,r.b,0,t));return o}(n,r)},n),e.$?s:h([e.a]))}),Ye=r(function(n,e){return m(n,{y:Vn(e),E:u(We,n.E,n.y,e)})}),ze=r(function(n,e){return a(ae,Mn(n),m(e,{e:a(Ye,e.e,n)}))}),Oe=r(function(n,e){return y(n,h([e]))}),Be=function(n){var e=n.y;return e.$?n:m(n,{y:re,z:a(Oe,n.z,e.a)})},Le=function(n){return n/1440|0},qe=r(function(n,e){var r=g(n,e)>-1?1+((n-e)/30|0):0;return p(e+30*r,r)}),He=Pn({c:"They seem wrong, somehow. Like something that shouldn't exist.",d:"You sense a bizarre otherworldly presence.",a:"WRONG",b:0}),Re=function(n){return n.b?p(n.a,n.b.b?n.b:s):p(He,s)},Me=r(function(n,e){for(;;){if(e<=0)return n;t=Re((r=n).z),n=m(r,{z:t.b,E:y(r.E,h([t.a]))}),e-=1}var r,t}),Pe=t(function(n,e,r){var t=a(qe,n,e);return p(t.a,a(Me,r,t.b))}),Fe=r(function(n,e){var r=u(Pe,n,e.X,e.e);return m(e,{e:r.b,f:n,X:r.a})}),Ie=r(function(n,e){return m(e,{Q:e.Q+n})}),De=r(function(n,e){return t=m(function(n){return m(n,{w:a(Ie,(e=n.e.E,u(In,r(function(n,e){return e+1}),0,e)+(n.e.y.$?0:1)),n.w)});var e}(a(ae,n,a(Fe,1440*Le(e.f)+1020,e))),{e:(o=e.e,m(i=Be(o),{z:y(i.z,i.E),E:s})),x:1}),a(ae,function(n){return r=(e=n.w).an,o=e.aj,a=e.Q,u=e.P,"Stats for the day: "+((t=e.ac)>0?"You sold items to "+te(t)+" customer(s). This netted you a total turnover of "+te(r)+"gp, "+(o>0?"and a profit of "+te(o)+"gp. ":o?"but you made a loss of "+te(o)+"gp! You should sell items for more than you bought them. ":"but you didn't make any profit! You should sell items for more than you bought them. "):"You didn't sell items to anyone, which is a bit worrying considering that you're a store. ")+(u>0?"You told "+te(e.P)+" customer(s) to fuck right off. ":"")+(a>0?"Your negligence resulted in "+te(e.Q)+" customer(s) leaving of their own accord.":"You managed to attend to every customer that entered your store, one way or another.");var e,r,t,o,a,u}(t),t);var t,o,i}),Je=r(function(n,e){return a(Fe,n+e.f,e)}),Qe=w,Xe=function(n){return a(Qe,1440,n)},Ge=r(function(n,e){var r=e+n;return g(Xe(r),1020)>-1||g(Xe(r),Xe(e))<0||g(r-e,1440)>0}),Ue=function(n){return m(n,{e:Be(n.e)})},Ke=function(n){return function(n){return m(n,{w:(e=n.w,m(e,{P:e.P+1}))});var e}(a(ae,1===(r=(e=n.e).y).$?"Who are you kicking out?":"You tell "+r.a.a+" to fuck off. They get angry, and leave the store, taking "+te(e.aw)+" minutes.",Ue(function(n){return!n.x&&a(Ge,n.e.aw,n.f)?a(De,"Whilst you tell them to fuck off, the store closes anyway",n):n.e.y.$?n:a(Je,n.e.aw,n)}(n))));var e,r},Ve=function(n){switch(n){case 0:return 80;case 1:return 100;case 2:return 150;case 3:return 200;default:return 300}},Ze=r(function(n,e){return m(e,{p:n})}),nr=r(function(n,e){return m(e,{h:a(Ze,n,e.h)})}),er=function(n){return g(n.M,3)<0?m(n,{M:n.M+1}):n},rr=r(function(n,e){return e.$?re:Vn(n(e.a))}),tr=r(function(n,e){return n.bh*Ve(e.b)*(100+50*e.M)/1e4|0}),or=r(function(n,e){return"The customer, "+n.a+", rejected the sales price of "+te(e.n)+"gp for the "+e.p.T+", taking "+te(10)+" minutes."}),ar=t(function(n,e,r){return a(Ge,10,r.f)?a(De,"Unfortunately, before the client can reject your offer, the store closes, and you are forced to shoo them out.",r):a(ae,a(or,n,e),a(Je,10,r))}),ur=r(function(n,e){return"The customer, "+n.a+", bought 1 "+e.p.T+" at "+te(e.n)+"gp (cost price "+te(e.p.bh)+"gp), and leaves happy, taking "+te(5)+" minutes."}),ir=r(function(n,e){return"You offered the "+(r=e).p.T+" for a price of "+te(r.n)+"gp.\n"+a(ur,n,e);var r}),cr=t(function(n,e,r){return m(r,{aj:r.aj+(n-e),an:r.an+n})}),fr=t(function(n,e,r){return m(r,{w:u(cr,n,e,r.w)})}),sr=r(function(n,e){return!e.x&&a(Ge,5,e.f)?a(De,"The customer looked just about ready to buy the item! But unfortunately, what with the curfew and all, you have to tell them to come back tomorrow.",e):function(n){return m(n,{w:(e=n.w,m(e,{ac:e.ac+1}))});var e}(Ue(u(fr,e.h.n,e.h.p.bh,function(n){return m(n,{ai:n.ai+n.h.n})}(a(ae,a(ir,n,e.h),a(Je,5,e))))))}),lr=r(function(n,e){return e.$?n:e.a}),dr=function(n){for(var e=0,r=n.charCodeAt(0),t=43==r||45==r?1:0,o=t;o<n.length;++o){var a=n.charCodeAt(o);if(a<48||57<a)return re;e=10*e+a-48}return o==t?re:Vn(45==r?-e:e)},hr=r(function(n,e){return m(e,{n:a(pe,0,a(lr,e.n,dr(n)))})}),vr=r(function(n,e){return m(e,{h:a(hr,n,e.h)})}),br=r(function(n,e){return m(e,{B:a(pe,0,a(lr,e.B,dr(n)))})}),gr=r(function(n,e){switch(n.$){case 0:return p(e,Ce);case 15:return p(function(n){return m(n,{t:4})}(e),Ce);case 1:return p(function(n){return m(n,{t:5})}(e),Ce);case 2:return p(a(vr,n.a,e),Ce);case 3:return p(function(n){var e=n.e.y;if(e.$)return a(ae,"Please address a customer before submitting an offer.",n);var r=e.a;return g(n.h.n,a(tr,n.h.p,r))<1?a(sr,r,n):u(ar,r,n.h,n)}(e),Ce);case 4:return p(m(e,{O:s}),Ce);case 5:return p(function(n){return m(n,{t:1})}(e),Ce);case 6:return p(Ke(e),Ce);case 7:return p(function(n){return m(n,{t:0})}(e),Ce);case 8:return p(function(n){return!n.x&&a(Ge,n.ab,n.f)?a(De,"The store closes whilst you are busy cleanings.",n):a(ae,"You clean the store for "+te(n.ab)+" minutes.",a(Je,n.ab,n))}(e),Ce);case 9:return p(m(e,{ad:!e.ad}),Ce);case 17:return p(function(n){return m(n,{t:3})}(e),Ce);case 18:return p(function(n){if(!n.x&&a(Ge,10,n.f))return a(De,"Whilst you are busy staring at the customer, the store closes, and they leave before you can get a better look.",n);var e,r,t=n.e.y;return t.$?a(ae,"Who are you trying to inspeect?",n):a(ae,"You inspect "+(e=t.a).a+" for "+te(10)+" minutes. "+e.c+" "+(r=e.b,function(){switch(r){case 0:return"They seem pretty much destitute; ";case 1:return"They seem quite poor; ";case 2:return"They seem to be of average wealth; ";case 3:return"They seem to be quite well-off; ";default:return"They appear to be rather wealthy; "}}()+"they'd probably pay about "+te(Ve(r))+"% of the item's value without being schmoozed."),a(Je,10,n))}(e),Ce);case 10:return p(function(n){return m(n,{t:2})}(e),Ce);case 11:return p(function(n){if(!n.x&&a(Ge,20,n.f))return a(De,"Whilst you are busy schmoozing the customer, the store closes, and you are forced to schmoo them out.",n);var e,r,t=n.e.y;return t.$?a(ae,"Who are you trying to schmooze?",n):m(e=a(ae,(o=t.a,g(o.M,3)<0?"You tell "+o.a+" that they have lovely hair. They are impressed and are willing to pay 50% more for the item. This takes "+te(20)+" minutes.":"You tell "+o.a+" that they have lovely hair. They snap at you that you've said that "+te(3)+" times already. They seem annoyed. This takes "+te(20)+" minutes."),a(Je,20,n)),{e:(r=e.e,m(r,{y:a(rr,er,r.y)}))});var o}(e),Ce);case 12:return p(a(ze,n.a,e),Ce);case 13:return p(function(n){return m(n,{h:(e=n.h,m(e,{n:e.p.bh}))});var e}(e),Ce);case 14:return p(a(br,n.a,e),Ce);case 16:return p(function(n){return!n.x&&a(Ge,n.B,n.f)?a(De,"Whilst you are busy sitting on your ass, you lose track of the time, and next thing you know, the store is closed and the customers have left.",n):a(ae,"You sit on your ass for "+te(n.B)+" minutes.",a(Je,n.B,n))}(e),Ce);case 19:return p(function(n){return m(n,{t:6})}(e),Ce);case 20:return p(function(n){return a(ae,Mn(Un),(e=function(n){return m(n,{e:(e=n.e,r=Re(e.z),t=r.b,m(e,{y:Vn(r.a),z:t})),t:5,w:ne,x:0,f:1440*(Le(n.f)+1)+540,X:1440*(Le(n.f)+1)+540+30});var e,r,t}(n),a(ae,"You open your doors on day "+te(Le(e.f))+" of many!",e)));var e}(e),Ce);default:return p(a(nr,n.a,e),Ce)}}),pr=C,mr=function(n){return{$:0,a:n}},yr=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},wr=sn,$r=a(wr,"border","2px solid black"),kr=un("div"),Ar=un("h2"),xr=an,jr={$:4},Tr={$:9},Sr=un("button"),Er=un("h3"),_r=un("textarea"),Cr=r(function(n,e){return a(dn,function(n){return/^(on|formAction$)/i.test(n)?"data-"+n:n}(n),function(n){return/^\s*(javascript:|data:text\/html)/i.test(n)?"":n}(e))}),Nr=H,Wr=r(function(n,e){return a(ln,n,Nr(e))})("readOnly"),Yr=fn,zr=r(function(n,e){return a(Yr,n,{$:0,a:e})}),Or=function(n){return a(zr,"click",mr(n))},Br=r(function(n,e){return a(Sr,a(qn,a(wr,"margin","2px"),n),e)}),Lr=h([a(Er,s,h([xr("Actions")])),a(kr,s,h([a(Br,h([Or({$:19})]),h([xr("Open Store")]))]))]),qr=h([a(Er,s,h([xr("Actions")])),a(kr,s,h([a(Br,h([Or({$:1})]),h([xr("Sale")])),a(Br,h([Or({$:10})]),h([xr("Schmooze")])),a(Br,h([Or({$:17})]),h([xr("Inspect")])),a(Br,h([Or({$:5})]),h([xr("Kick Out")])),a(Br,h([Or({$:7})]),h([xr("Clean")])),a(Br,h([Or({$:15})]),h([xr("Wait")]))]))]),Hr={$:20},Rr={$:8},Mr={$:18},Pr={$:6},Fr=function(n){return{$:2,a:n}},Ir={$:13},Dr={$:11},Jr={$:3},Qr=function(n){return{$:14,a:n}},Xr={$:16},Gr=r(function(n,e){return"Sell "+e.p.T+" (cost "+te(e.p.bh)+"gp) to "+n.a+" for "+te(e.n)+"gp."}),Ur=r(function(n,e){return a(Br,h([Or(Fr(te(e.h.n+n)))]),h([xr(y(n>0?"+":"",te(n)))]))}),Kr=r(function(n,e){return a(Br,h([Or(Qr(te(e.B+n)))]),h([xr(y(n>0?"+":"",te(n)))]))}),Vr=un("br"),Zr=un("input"),nt=H,et=r(function(n,e){return a(ln,n,nt(e))}),rt=et("max"),tt=et("min"),ot=et("type"),at=et("value"),ut=function(n){return p(n,!0)},it=r(function(n,e){return a(Yr,n,{$:1,a:e})}),ct=E,ft=S,st=a(r(function(n,e){return u(Qn,ct,e,n)}),h(["target","value"]),ft),lt=function(n){return a(it,"input",a(pr,ut,a(pr,n,st)))},dt=h([a(Er,s,h([xr("Customers")])),a(kr,s,h([xr("The store is now closed.")]))]),ht=r(function(n,e){return a(Xn,function(e){return a(Sr,h([n(e)]),h([xr(e.a)]))},e.E)}),vt=function(n){return a(kr,h([a(wr,"float","left"),a(wr,"width","50%")]),h([a(kr,h([$r,a(wr,"margin-top","5px"),a(wr,"margin-bottom","0px"),a(wr,"margin-left","5px"),a(wr,"margin-right","5px"),a(wr,"padding-top","0px"),a(wr,"padding-bottom","10px"),a(wr,"padding-left","0px"),a(wr,"padding-right","0px")]),n)]))},bt=function(n){return{$:21,a:n}},gt={T:"packet of trail mix",bh:5},pt=function(){return h([a(Er,s,h([xr("Stock")])),a(Br,h([Or(bt(ee))]),h([xr("Sword")])),a(Br,h([Or(bt(gt))]),h([xr("Bag of trail mix")]))])},mt=r(function(n,e){return r=h(n?[vt(pt()),vt(Lr),vt(dt),vt(function(n){return h([a(Er,s,h([xr("Current Action")])),function(){switch(n.t){case 0:case 1:case 2:case 3:case 5:case 4:return a(kr,s,h([xr("The store is now closed.")]));default:return a(kr,s,h([a(Br,h([Or(Hr)]),h([xr("Skip until tomorrow and open store at "+te(9)+" o Clock.")]))]))}}()])}(e))]:[vt(pt()),vt(qr),vt(function(n){return h([a(Er,s,h([xr("Customers")])),a(kr,s,a(ht,function(n){return Or({$:12,a:n})},n.e)),a(Vr,s,s),a(kr,s,h([xr("You are speaking to: "+(e=n.e.y,(e.$?"No-one":e.a.a)+"."))]))]);var e}(e)),vt(function(n){return h([a(Er,s,h([xr("Current Action")])),function(){switch(n.t){case 0:return a(kr,s,h([a(Br,h([Or(Rr)]),h([xr("Clean Store")]))]));case 1:var e=n.e.y;if(1===e.$)return a(kr,s,h([xr("Select a customer to address.")]));var r=e.a;return a(kr,s,h([a(Br,h([Or(Pr)]),h([xr("Kick out "+r.a)]))]));case 2:var t=n.e.y;return 1===t.$?a(kr,s,h([xr("Select a customer to address.")])):(r=t.a,a(kr,s,h([a(Br,h([Or(Dr)]),h([xr("Schmooze "+r.a)]))])));case 3:var o=n.e.y;return 1===o.$?a(kr,s,h([xr("Select a customer to address.")])):(r=o.a,a(kr,s,h([a(Br,h([Or(Mr)]),h([xr("Inspect "+r.a)]))])));case 5:var u=n.e.y;return 1===u.$?a(kr,s,h([xr("Select a customer to address.")])):(r=u.a,a(kr,s,h([a(kr,h([a(wr,"margin-bottom","5px")]),h([a(kr,s,h([a(Br,h([Or(Ir)]),h([xr("Reset")])),a(Ur,-100,n),a(Ur,-10,n),a(Zr,h([a(Cr,"aria-label","Price in gold"),a(wr,"margin","2px"),ot("number"),tt("0"),rt("50000"),at(te(n.h.n)),lt(Fr)]),s),a(Ur,10,n),a(Ur,100,n)])),a(Vr,s,s),a(kr,s,s),a(Br,h([Or(Jr)]),h([xr(a(Gr,r,n.h))]))]))])));case 4:return a(kr,s,h([a(Br,h([Or(Qr(te(0)))]),h([xr("Reset")])),a(Kr,-60,n),a(Kr,-10,n),a(Zr,h([a(Cr,"aria-label","Time to wait"),a(wr,"margin","2px"),ot("number"),tt("0"),rt("1440"),at(te(n.B)),lt(Qr)]),s),a(Kr,10,n),a(Kr,60,n),a(kr,s,s),a(Br,h([Or(Xr)]),h([xr("Wait for "+te(n.B)+" minutes.")]))]));default:return a(kr,s,h([xr("The store must be closed for you to be able to open it.")]))}}()])}(e))]),a(kr,h([a(wr,"width","100%"),a(wr,"clear","both"),a(wr,"display","table")]),h([a(kr,h([a(wr,"margin-top","5px"),a(wr,"margin-bottom","10px"),a(wr,"margin-left","5px"),a(wr,"margin-right","5px")]),r)]));var r}),yt=un("h1"),wt=M,$t=wt(0),kt=F,At=r(function(n,e){return a(kt,function(e){return wt(n(e))},e)}),xt=t(function(n,e,r){return a(kt,function(e){return a(kt,function(r){return wt(a(n,e,r))},r)},e)}),jt=Z,Tt=r(function(n,e){var r=e;return function(n){return P(function(e){e(M(D(n)))})}(a(kt,jt(n),r))});U.Task={b:$t,c:t(function(n,e){return a(At,function(){return 0},(r=a(Xn,Tt(n),e),u(Qn,xt(qn),wt(s),r)));var r}),d:t(function(){return wt(0)}),e:r(function(n,e){return a(At,n,e)}),f:void 0};var St,Et=_,_t=T;St={Main:{init:On({bg:function(n){return p(a(ae,Mn(Un),{ab:10,O:s,e:Zn,ad:!0,h:{p:ee,n:20},ai:0,t:5,w:ne,x:0,f:540,X:570,B:0,_:n._}),Ce)},bn:r(function(n){return n})(nn(s)),bp:gr,br:function(n){return a(kr,s,h([a(yt,s,h([xr("Trading Post")])),xr("https://mark-chimes.github.io/trading-post/"),a(Ar,s,h([xr("Debug")])),xr("Time of next customer "+oe(n.X)),a(kr,s,s),xr("minutesSinceZero: "+te(n.f)),a(kr,s,s),a(kr,s,h([xr("Customer max price: "+(r=n.e.y,r.$?"No Customer":te(a(tr,n.h.p,r.a))))])),(e=function(n){return h([a(Ar,s,h([xr("Store")])),a(kr,s,h([xr("Time: "+oe(n.f))])),a(kr,s,h([xr("Day: "+te(Le(n.f)))])),a(kr,s,h([xr("Your gold: "+te(n.ai)+"gp")]))])}(n),a(kr,h([a(wr,"width","100%")]),h([a(kr,h([$r,a(wr,"margin-bottom","5px"),a(wr,"margin-left","10px"),a(wr,"margin-right","10px"),a(wr,"padding-bottom","10px")]),e)]))),a(mt,n.x,n),function(n){return a(kr,h([a(wr,"width","100%"),a(wr,"clear","both"),a(wr,"display","table")]),h([a(kr,h([$r,a(wr,"margin-top","10px"),a(wr,"margin-bottom","5px"),a(wr,"margin-left","10px"),a(wr,"margin-right","10px"),a(wr,"padding-bottom","10px")]),n)]))}(function(n){return h([a(Er,s,h([xr("The story thus far: ")])),a(Sr,h([Or(jr)]),h([xr("Clear Story")])),a(Sr,h([Or(Tr)]),h([xr("Reverse Story")])),a(kr,s,s),a(_r,h([a(Cr,"aria-live","assertive"),Wr(!0),(r=n._/10|0,a(dn,"cols",te(r))),a(dn,"rows",te(15))]),(n.ad?Dn:function(n){return n})(a(Xn,function(n){return xr(n+"\n")},(e=n.O,a(Xn,function(n){return a(_e,"\n",n)},e)))))]);var e,r}(n))]));var e,r}})(a(Et,function(n){return mr({_:n})},a(ct,"windowWidth",_t)))(0)}},n.Elm?function n(e,r){for(var t in r)t in e?"init"==t?v(6):n(e[t],r[t]):e[t]=r[t]}(n.Elm,St):n.Elm=St}(this)},function(n,e,r){r(3),n.exports=r(11)},,,,,,,,function(){},function(n,e,r){"use strict";r.r(e),r(10);var t=r(1),o=!("localhost"!==window.location.hostname&&"[::1]"!==window.location.hostname&&!window.location.hostname.match(/^127(?:\.(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3}$/));function a(n,e){navigator.serviceWorker.register(n).then(function(n){n.onupdatefound=function(){var r=n.installing;null!=r&&(r.onstatechange=function(){"installed"===r.state&&(navigator.serviceWorker.controller?(console.log("New content is available and will be used when all tabs for this page are closed. See https://bit.ly/CRA-PWA."),e&&e.onUpdate&&e.onUpdate(n)):(console.log("Content is cached for offline use."),e&&e.onSuccess&&e.onSuccess(n)))})}}).catch(function(n){console.error("Error during service worker registration:",n)})}t.Elm.Main.init({node:document.getElementById("root"),flags:{windowWidth:window.innerWidth}}),function(){if("serviceWorker"in navigator){if(new URL("/trading-post",window.location.href).origin!==window.location.origin)return;window.addEventListener("load",function(){var n="".concat("/trading-post","/service-worker.js");o?(function(n){fetch(n).then(function(e){var r=e.headers.get("content-type");404===e.status||null!=r&&-1===r.indexOf("javascript")?navigator.serviceWorker.ready.then(function(n){n.unregister().then(function(){window.location.reload()})}):a(n,void 0)}).catch(function(){console.log("No internet connection found. App is running in offline mode.")})}(n),navigator.serviceWorker.ready.then(function(){console.log("This web app is being served cache-first by a service worker. To learn more, visit https://bit.ly/CRA-PWA")})):a(n,void 0)})}}()}],[[2,1,2]]]);
//# sourceMappingURL=main.be472fa7.chunk.js.map