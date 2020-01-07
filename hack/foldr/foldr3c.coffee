S=console.log;t=(v)->->v
c=(v)->l=[t v];(l.n=->l.push t v;l.shift());l
f=(s,a,l)->if l.length==0then a else s l.n(),->f s,a,l
S f ((x,a)->x()||a()),false,c true;S f ((x,a)->a()||x()),false,c true
