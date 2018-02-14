cd('C:\Users\Captain Bluebear\Desktop\TP\Year 4\Semester 2 - Hillary\MA4492 - Project\MatLab')

addpath('SaliencyToolbox')


%batchSaliency - batch processing of lists of images.
% Set the images to be processeed. Here all frames are in
% the folder 'images'.
images = 'images';

%The number of fixations to be calculated under lower image feature
%value.
numFixations = 3;

[salMaps,fixations] = batchSaliency(images,numFixations)

%Write the predicted saliency map to csv files.
for i = 1:460
    display(i)
    file_name = ['salMap_', num2str(i), '.csv'];
    csvrite(file_name, salMaps(i).data)
    fix_file = ['fixation_sal', num2str(i), '.csv'];
    csvwrite(file_name, fixations(i))
end
