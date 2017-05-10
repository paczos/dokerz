for i in *; do [ -d $i ] && { cd $i; docker build -t 42n4/$i . ; docker push 42n4/$i; cd .. ; }  done
