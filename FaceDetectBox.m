% face detector
% outputs face coordinate, size of box containing face

% Create a cascade detector object.
faceDetector = vision.CascadeObjectDetector();

% Read a video frame and run the detector.
videoFileReader = vision.VideoFileReader('megamind_l.mpeg');

i = 1;

while i < 461
    %videoFrame      = step(videoFileReader);
    display(i);
    if length(num2str(i)) == 1
        videoFileName = ['images/00', num2str(i) '.jpg'];
    elseif length(num2str(i)) == 2
        videoFileName = ['images/0', num2str(i) '.jpg'];
    else
        videoFileName = ['images/', num2str(i) '.jpg'];
    end   
    videoFrame = imread(videoFileName);
    
    %the box containing the face
    bbox            = step(faceDetector, videoFrame);
    
    %the centroid of the box
    centroids = [round(bbox(:, 1) + bbox(:, 3) / 2), ...
    round(bbox(:, 2) + bbox(:, 4) / 2)];

    %box size
    box_size = [bbox(:,3), bbox(:,4)];

    file_name_box = ['FaceBox_', num2str(i), '.csv'];
    file_name_centroid = ['FaceCentroid_', num2str(i), '.csv'];
    file_name_box_size = ['FaceBoxSize_', num2str(i), '.csv'];
    
    i = i + 1;
    
    csvwrite(file_name_box, bbox);
    csvwrite(file_name_centroid, centroids);
    csvwrite(file_name_box_size, box_size);
end