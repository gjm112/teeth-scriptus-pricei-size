wd = "/Users/gregorymatthews/Dropbox"
cd /Users/gregorymatthews/Dropbox/teeth-scriptus-pricei-size/code/matlab_size/

% get data from bushbuck
data_bushbuck = readtable(wd+"/teeth-scriptus-pricei/data/matlab/data_bushbuck.csv");
        %get the number of rows and cols
        n_rows = size(data_bushbuck,1);
        n_cols = size(data_bushbuck,2);
        
        %blank array storage for matrix for each image
        teeth_data_bushbuck = zeros(2,100,n_rows/2);
        
        
        %loop to assign each tooth's coordinates to the appropriate spot in the
        %array
        for i=1:2:n_rows
            %get index for the image we are on 
            j = find((1:2:n_rows)==i);
            
            %assign matrix for image j 
             X = data_bushbuck{i:(i+1), 2:n_cols};
            
            %resample so all of the curves have 100 points
            teeth_data_bushbuck(:,:,j) = ReSampleCurve(X,100);
        end



%Get the LM3's fromscriptus and pricei
toothtype = {"LM3"};
species = {"pricei", "scriptus"};
for t=1
  data_scriptus = readtable(wd + "/teeth-scriptus-pricei-size/data/matlab/data_"+toothtype(t)+"_scriptus.csv")
  data_pricei = readtable(wd + "/teeth-scriptus-pricei-size/data/matlab/data_"+toothtype(t)+"_pricei.csv")
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

    end

ddd_bushbuck(67,68)  = GeodesicElasticClosed(teeth_data_bushbuck(:,:,1),teeth_data_bushbuck(:,:,4));     
ddd_bushbuck(68,67,) = ddd_bushbuck(67,68)

load(wd + "/teeth-scriptus-pricei-size/data/matlab/pairwise_distances_"+toothtype(t)+".mat","ddd")

size(ddd,1)
ddd_bushbuck = ddd;

%SK4261
for i = 1:size(ddd,1);
    i
    ddd_bushbuck(i,size(ddd,1)+1)  = GeodesicElasticClosed(teeth_data(:,:,i),teeth_data_bushbuck(:,:,1));     
    ddd_bushbuck(size(ddd,1)+1,i) = ddd_bushbuck(i,size(ddd,1)+1);
    ddd_bushbuck(size(ddd,1)+1,i)
end

%UW88519a
for i = 1:size(ddd,1);
    i
    ddd_bushbuck(i,size(ddd,1)+2)  = GeodesicElasticClosed(teeth_data(:,:,i),teeth_data_bushbuck(:,:,4));     
    ddd_bushbuck(size(ddd,1)+2,i) = ddd_bushbuck(i,size(ddd,1)+2);
    ddd_bushbuck(size(ddd,1)+2,i)
end


save(wd + "/teeth-scriptus-pricei-size/data/matlab/pairwise_distances_bushbuck.mat","ddd_bushbuck");
csvwrite(wd + "/teeth-scriptus-pricei-size/data/matlab/pairwise_distances_bushbuck.csv",ddd_bushbuck);