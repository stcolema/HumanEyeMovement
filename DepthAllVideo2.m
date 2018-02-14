%The basis of this code can be found at:
%http://uk.mathworks.com/help/vision/examples/uncalibrated-stereo-image-rectification.html

cd('C:\Users\Captain Bluebear\Desktop\TP\Year 4\Semester 2 - Hillary\MA4492 - Project\MatLab')

image2 = imread('images_l\001.jpg');
image1 = imread('images_r\001.jpg');

% Convert to grayscale.
image1gray = rgb2gray(image1);
image2gray = rgb2gray(image2);

surfPoints1 = detectSURFFeatures(image1gray, 'MetricThreshold', 2000);
surfPoints2 = detectSURFFeatures(image2gray, 'MetricThreshold', 2000);

%Find point correspondences.
[features1, validBlobs1] = extractFeatures(image1gray, surfPoints1);
[features2, validBlobs2] = extractFeatures(image2gray, surfPoints2);

%Determine indices of matching features.
indexPairs = matchFeatures(features1, features2, 'Metric', 'SAD', ...
  'MatchThreshold', 5);

%Retrieve locations of matched points for each image
matchedPoints1 = validBlobs1(indexPairs(:,1),:);
matchedPoints2 = validBlobs2(indexPairs(:,2),:);


[fMatrix, epipolarInliers, status] = estimateFundamentalMatrix(...
  matchedPoints1, matchedPoints2, 'Method', 'RANSAC', ...
  'NumTrials', 10000, 'DistanceThreshold', 0.1, 'Confidence', 99.99);

if status ~= 0 || isEpipoleInImage(fMatrix, size(image1)) ...
  || isEpipoleInImage(fMatrix', size(image2))
  error(['Either not enough matching points were found or '...
         'the epipoles are inside the images. You may need to '...
         'inspect and improve the quality of detected features ',...
         'and/or improve the quality of your images.']);
end

inlierPoints1 = matchedPoints1(epipolarInliers, :);
inlierPoints2 = matchedPoints2(epipolarInliers, :);

%Estime camera parameters
[t1, t2] = estimateUncalibratedRectification(fMatrix, ...
  inlierPoints1.Location, inlierPoints2.Location, size(image2));
tform1 = projective2d(t1);
tform2 = projective2d(t2);


%Rectify the stereo images
[image1Rect, image2Rect] = rectifyStereoImages(image1, image2, tform1, tform2);


%Find disparity map
frameLeftGray  = rgb2gray(image1Rect);
frameRightGray = rgb2gray(image2Rect);
disparityMap = disparity(frameLeftGray, frameRightGray);

filename = ['disparityMap\disparityMap1.csv'];
csvwrite(filename, disparityMap);

%Carry out rectification across all frames and find disparity maps
i = 2;
while i <= 460
    if length(num2str(i)) == 1
        filenameLeft = ['images_l\00', num2str(i), '.jpg'];
        filenameRight = ['images_r\00', num2str(i), '.jpg'];
    elseif length(num2str(i)) == 2
        filenameLeft = ['images_l\0', num2str(i) '.jpg'];
        filenameRight = ['images_r\0', num2str(i), '.jpg'];
    else
        filenameLeft = ['images_l\', num2str(i) '.jpg'];
        filenameRight = ['images_r\', num2str(i), '.jpg'];
    end
    
    K1 = imread(filenameLeft);
    K2 = imread(filenameRight);

    [J1,J2] = rectifyStereoImages(K1,K2,tform1,tform2);

    %Goodness I dislike gray. Grey is so much nicer.
    frameLeftGray  = rgb2gray(J1);
    frameRightGray = rgb2gray(J2);
    disparityMap = disparity(frameLeftGray, frameRightGray);
    filename = ['disparityMap\disparityMap', num2str(i), '.csv'];
    csvwrite(filename, disparityMap);
    i = i + 1;
end