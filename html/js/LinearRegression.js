function LinearRegression(data){
	var X = [],
	    y = [];
	for (var i = 0; i < data.length; i ++){
        var tempX = [];
		for(var j = 0; j < data[0]['x'].length; j++){
            tempX.push(data[i]['x'][j]);
        }
        X.push(tempX);
		y.push([data[i].y]);

	}
	var t = matrixTranspose(X);
	var hat =  matrixMultiply(matrixMultiply(matrixInvert(matrixMultiply(t, X)), t), y);
	return hat;
}


function matrixTranspose(a){
	//http://stackoverflow.com/questions/4492678/to-swap-rows-with-columns-of-matrix-in-javascript-or-jquery
    return Object.keys(a[0]).map(
        function (c) { return a.map(function (r) { return r[c]; }); }
        );
    }

function matrixMultiply(m1, m2) {
	// http://tech.pro/tutorial/1527/matrix-multiplication-in-functional-javascript
	//
    var result = [];
    for (var i = 0; i < m1.length; i++) {
        result[i] = [];
        for (var j = 0; j < m2[0].length; j++) {
            var sum = 0;
            for (var k = 0; k < m1[0].length; k++) {
                sum += m1[i][k] * m2[k][j];
            }
            result[i][j] = sum;
        }
    }
    return result;
}

function matrixInvert(M){
	// source - http://blog.acipo.com/matrix-inversion-in-javascript/
	//
    if(M.length !== M[0].length){return;}
    

    var i=0, ii=0, j=0, dim=M.length, e=0, t=0;
    var I = [], C = [];
    for(i=0; i<dim; i+=1){
        I[I.length]=[];
        C[C.length]=[];
        for(j=0; j<dim; j+=1){
            
            if(i==j){ I[i][j] = 1; }
            else{ I[i][j] = 0; }
            
            C[i][j] = M[i][j];
        }
    }
    
    for(i=0; i<dim; i+=1){
        e = C[i][i];
        
        if(e==0){
            for(ii=i+1; ii<dim; ii+=1){
                if(C[ii][i] != 0){
                    for(j=0; j<dim; j++){
                        e = C[i][j];       
                        C[i][j] = C[ii][j];
                        C[ii][j] = e;      
                        e = I[i][j];       
                        I[i][j] = I[ii][j];
                        I[ii][j] = e;      
                    }
 
                    break;
                }
            }

            e = C[i][i];
            if(e==0){return}
        }
        
        for(j=0; j<dim; j++){
            C[i][j] = C[i][j]/e;
            I[i][j] = I[i][j]/e;
        }

        for(ii=0; ii<dim; ii++){
            if(ii==i){continue;}
            e = C[ii][i];
            for(j=0; j<dim; j++){
                C[ii][j] -= e*C[i][j];
                I[ii][j] -= e*I[i][j];
            }
        }
    }
    return I;
}
