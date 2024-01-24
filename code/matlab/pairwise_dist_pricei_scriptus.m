cd /Users/gregorymatthews/Dropbox/teeth-scriptus-pricei/code/matlab
toothtype = {"LM1","LM2","LM3","UM1","UM2","UM3"}
species = {"pricei", "scriptus"}
for t=1:6

        data_scriptus = readtable("/Users/gregorymatthews/Dropbox/teeth-scriptus-pricei/data/matlab/data_"+toothtype(t)+"_scriptus.csv")
        data_pricei = readtable("/Users/gregorymatthews/Dropbox/teeth-scriptus-pricei/data/matlab/data_"+toothtype(t)+"_pricei.csv")
data=[data_scriptus; data_pricei]
        
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
        
        cd /Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/code/matlab

         ddd = FindPairwiseDistance(teeth_data)

      %Now save the average tooth
        save("/Users/gregorymatthews/Dropbox/teeth-scriptus-pricei/data/matlab/pairwise_distances_"+toothtype(t)+".mat","ddd")
        
        csvwrite("/Users/gregorymatthews/Dropbox/teeth-scriptus-pricei/data/matlab/pairwise_distances_"+toothtype(t)+".csv",ddd)

end  
        
        

