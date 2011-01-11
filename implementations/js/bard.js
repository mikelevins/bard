// an experiment in JavaScript

function isNormalForm(exp,env){return true}

function reduce(exp,env){return[0,1]}

function runVM(exp, env){
	if(isNormalForm(exp,env)){
		return [exp,env]
	}else{
		var result = reduce(exp,env)
		return runVM(result[0],result[1])
	}
}