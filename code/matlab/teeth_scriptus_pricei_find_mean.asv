

data_LM1_scriptus = readtable("/Users/gregorymatthews/Dropbox/teeth-scriptus-pricei/data/matlab/data_LM1_scriptus.csv")

%get the number of rows and cols
n_rows = size(data_LM1_scriptus,1);
n_cols = size(data_LM1_scriptus,2);

%blank array storage for matrix for each image
teeth_data = zeros(2,100,n_rows/2);


%loop to assign each tooth's coordinates to the appropriate spot in the
%array
for i=1:2:n_rows
    %get index for the image we are on 
    j = find((1:2:n_rows)==i);
    
    %assign matrix for image j 
     X = data_LM1_scriptus{i:(i+1), 2:n_cols};
    
    %resample so all of the curves have 100 points
    teeth_data(:,:,j) = ReSampleCurve(X,100);
end

%Compute mean
cd /Users/gregorymatthews/Dropbox/teeth-scriptus-pricei/code/matlab/
[out_q,q,E] = FindElasticMeanFast(teeth_data)
out_beta = q_to_curve(out_q)


%Now save the average tooth
cd /Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data
save('out_beta.mat')





