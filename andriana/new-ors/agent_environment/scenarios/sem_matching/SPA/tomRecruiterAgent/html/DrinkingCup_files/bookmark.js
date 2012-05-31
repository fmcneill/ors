
var addBookmarkObj = {
  linkText:'Bookmark this link',
  addTextLink:function(param){
    var a=addBookmarkObj.makeLink(param);
    if(!a) return;
    a.appendChild(document.createTextNode(addBookmarkObj.linkText));
  },
  makeLink:function(param) {
    if(!document.getElementById || !document.createTextNode) return null;
    param=((typeof(param)=='string')&&!isEmpty(param))
      ?param:'tool-book';
    var cont=document.getElementById(param);
    if(!cont) return null;
    var a=document.createElement('a');
    a.href=location.href;
    if(window.opera) {
      a.rel='sidebar'; // Opera 7+
    } else {
      // not Opera.
      a.onclick=function() {
        addBookmarkObj.exec(this.href,this.title);
        return false;
      }
    }
    a.title=document.title;
    return cont.appendChild(a);
  },
  exec:function(url, title) {
    var ua=navigator.userAgent.toLowerCase();
    var isKonq=(ua.indexOf('konqueror')!=-1);
    var isSafari=(ua.indexOf('webkit')!=-1);
    var isMac=(ua.indexOf('mac')!=-1);
    var buttonStr=isMac?'Command/Cmd':'CTRL';

    if(window.external && (!document.createTextNode ||
      (typeof(window.external.AddFavorite)=='unknown'))) {
        window.external.AddFavorite(url, title); // IE/Win
    } else if(isKonq) {
      alert('You need to press CTRL + B to bookmark this site.');
    } else if(window.opera) {
      void(0); // do nothing here (Opera 7+)
    } else if(window.home || isSafari) { // Firefox, Netscape, Safari, iCab
      alert('You need to press '+buttonStr+' + D to bookmark our site.');
    } else if(!window.print || isMac) { // IE5/Mac and Safari 1.0
      alert('You need to press Command/Cmd + D to bookmark this site.');    
    } else {
      alert('In order to bookmark this site you need to do so manually '+
        'through your browser.');
    }
  }
}

function isEmpty(s){return ((s=='')||/^\s*$/.test(s));}

function nx_addWindowEvent(el,etype,fn) {
  if(el.addEventListener && (!window.opera || opera.version) && (etype!='load')) {
    el.addEventListener(etype,fn,false);
  } else if(el.attachEvent) {
    el.attachEvent('on'+etype,fn);
  } else {
    if(typeof(fn) != "function") return;
    if(typeof(window.earlyNS4)=='undefined') {
      // Netscape versions before 4.02
      window.earlyNS4=((navigator.appName.toLowerCase()=='netscape')&&
      (parseFloat(navigator.appVersion)<4.02)&&document.layers);
    }
    if((typeof(el['on'+etype])=="function")&&!window.earlyNS4) {
      var tempFunc = el['on'+etype];
      el['on'+etype]=function(e){
        var a=tempFunc(e),b=fn(e);
        a=(typeof(a)=='undefined')?true:a;
        b=(typeof(b)=='undefined')?true:b;
        return (a&&b);
      }
    } else {
      el['on'+etype]=fn;
    }
  }
}

nx_addWindowEvent(window,'load',addBookmarkObj.addTextLink);