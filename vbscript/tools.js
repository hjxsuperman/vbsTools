
//检查是否是单标签(可以忽略)
function checkRender(tag){
	var list=['br','hr','img','param','meta','link']
	for(var i=0;i<list.length;i++){
		if(tag.toLowerCase()==list[i]){
			return true;
			break;
		}
	}
	return false;
}
//获得渲染数据(可以忽略)
function render(tree) {
		//解析1 渲染10w条数据，耗时5s
		if(tree){
			var tag=document.createElement(tree.tag[0])
			if(tree.tag[1]){
				for(var key in tree.tag[1]){
					tag.setAttribute(key,tree.tag[1][key])
				}
			}
			if(tree.tag[2]){
				tag.innerHTML=tree.tag[2]
			}
			if(tree.children instanceof Array){
				for(var x in tree.children){
					tag.appendChild(render(tree.children[x]))
				}
			}
			return tag
		} 
		//解析2 渲染只需2s多一点
		/*if(tree){
			var tag="<"+tree.tag[0] 
			var isSingle=checkRender(tag)
			//console.log(tree.tag.at(1))
			if(tree.tag[0]){
				for(var key in tree.tag[1]){
					tag=tag+key+"='"+tree.tag[1][key]+"'"
				}
			}
			tag=isSingle?tag+' />':tag+">"
			if(tree.tag[2]){
				tag=isSingle?"":tag+tree.tag[2]
			}
			if(tree.child instanceof Array){
				for(var x in tree.child){
					tag=tag+render(tree.child[x])
					return tag
				}
			}
			return tag+(!isSingle?"</"+tree.tag[0]+">":"")
		}*/
	}
 //挂载(主要使用)
function mouted(element,tree){
	var renderNode=render(tree)
	var myele=typeof element=="string"?dom(element):element
	//实现1
	myele.innerHTML=""
	myele.appendChild(renderNode)
	//实现1
	/*//实现2
	myele.innerHTML=renderNode
	*/
}

//获取dom节点(主要使用)
function dom(eleName){
	var list=['#',"."]
	var index=eleName.slice(0,1)==list[0]?0:eleName.slice(0,1)==list[1]?1:-1
	if(index==0){
		return document.getElementById(eleName.slice(1))
	}else if(index==1){
		return document.getElementsByClassName(eleName.slice(1))
	}else{
		return document.getElementsByTagName(eleName)
	}
}