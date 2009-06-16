
function showInfoList(e)
{
    var parent = e.parentNode;
    var children = parent.childNodes;
    var len = children.length;
    
    for(var i = 0; i < len; i++){
    	if (children[i].nodeName == "UL"
	    && children[i].className == "clickable"){
	    var item = children[i];
	    if (item.style.display != "block"){
		item.style.display = "block";
	    } else{
		item.style.display = "none";
	    }
	}
    }
}
