from tensorflow.keras.models import load_model
from tensorflow.keras.preprocessing import image
from tensorflow.keras.applications.mobilenet_v2 import preprocess_input, decode_predictions
import numpy as np
from PIL import Image

model = load_model('Alan_model.h5')

class_labels = {0: 'Speed limit (20km/h)',
                1: 'Speed limit (30km/h)',
                2: 'Speed limit (50km/h)',
                3: 'Speed limit (60km/h)',
                4: 'Speed limit (70km/h)',
                5: 'Speed limit (80km/h)',
                6: 'End of speed limit (80km/h)',
                7: 'Speed limit (100km/h)',
                8: 'Speed limit (120km/h)',
                9: 'No passing',
                10: 'No passing for vehicles over 3.5 metric tons',
                11: 'Right-of-way at the next intersection',
                12: 'Priority road',
                13: 'Yield',
                14: 'Stop',
                15: 'No vehicles',
                16: 'Vehicles over 3.5 metric tons prohibited',
                17: 'No entry',
                18: 'General caution',
                19: 'Dangerous curve to the left',
                20: 'Dangerous curve to the right',
                21: 'Double curve',
                22: 'Bumpy road',
                23: 'Slippery road',
                24: 'Road narrows on the right',
                25: 'Road work',
                26: 'Traffic signals',
                27: 'Pedestrians',
                28: 'Children crossing',
                29: 'Bicycles crossing',
                30: 'Beware of ice/snow',
                31: 'Wild animals crossing',
                32: 'End of all speed and passing limits',
                33: 'Turn right ahead',
                34: 'Turn left ahead',
                35: 'Ahead only',
                36: 'Go straight or right',
                37: 'Go straight or left',
                38: 'Keep right',
                39: 'Keep left',
                40: 'Roundabout mandatory',
                41: 'End of no passing',
                42: 'End of no passing by vehicles over 3.5 metric tons'}
top_n = 3


def classify_image(img_path):
    img = image.load_img(img_path, target_size=(32, 32))
    image_gray = img.convert('L')
    img_array = image.img_to_array(image_gray)
    img_array = np.expand_dims(img_array, axis=0)
    img_array = preprocess_input(img_array)
    predictions = model.predict(img_array)
    top_indices = np.argsort(predictions[0])[-top_n:][::-1]

    # Map indices to class labels
    decoded_predictions = [(class_labels[i], predictions[0][i]) for i in top_indices]
    return decoded_predictions


img_path = 'S4.jpg'

predictions = classify_image(img_path)
print(f"The Traffic  sign says '{predictions[0][0]}'")
print(f'Accuracy:{predictions[0][1]}')