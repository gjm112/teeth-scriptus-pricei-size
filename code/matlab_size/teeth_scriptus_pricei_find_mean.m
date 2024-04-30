%cd /Users/nastaranghorbani/Documents/size/code/matlab
cd /Users/nastaranghorbani/Documents/size/code/matlab_size
toothtype = {"LM1","LM2","LM3","UM1","UM2","UM3"}
species = {"pricei", "scriptus"}
for t=1:6
    for sp=1:2
        data = readtable("/Users/nastaranghorbani/Documents/size/data/matlab/data_"+toothtype(t)+"_"+species(sp)+".csv")
        
        %get the number of rows and cols
        n_rows = size(data,1);
        n_cols = size(data,2);
        
        %blank array storage for matrix for each image
        teeth_data = zeros(2,100,n_rows/2);
        
        
        %loop to assign each tooth's coordinates to the appropriate spot in the
        %array
        for i=1:2:n_rows
            %get index for the image we are on 
            j = find((1:2:n_rows)==i);
            
            %assign matrix for image j 
             X = data{i:(i+1), 2:n_cols};
            
            %resample so all of the curves have 100 points
            teeth_data(:,:,j) = ReSampleCurve(X,100);
        end
        
        %Compute mean
        [out_q,q,E] = FindElasticMeanFast(teeth_data)
        out_beta = q_to_curve(out_q)
        [VV,PC_feat]=FindTangentFeatures(out_q,q,10)


        
        %Now save the average tooth
        save("/Users/nastaranghorbani/Documents/size/data/matlab/VV_"+toothtype(t)+"_"+species(sp)+".mat","VV")
        save("/Users/nastaranghorbani/Documents/size/data/matlab/PC_feat_"+toothtype(t)+"_"+species(sp)+".mat","PC_feat")
        save("/Users/nastaranghorbani/Documents/size/data/matlab/out_beta_"+toothtype(t)+"_"+species(sp)+".mat","out_beta")

csvwrite("/Users/nastaranghorbani/Documents/size/data/matlab/VV_"+toothtype(t)+"_"+species(sp)+".csv",VV)
csvwrite("/Users/nastaranghorbani/Documents/size/data/matlab/PC_feat_"+toothtype(t)+"_"+species(sp)+".csv",PC_feat)
csvwrite("/Users/nastaranghorbani/Documents/size/data/matlab/out_beta_"+toothtype(t)+"_"+species(sp)+".csv",out_beta)

    end
end  
        
        

