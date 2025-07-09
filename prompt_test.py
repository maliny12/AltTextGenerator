import os
import base64
from openai import OpenAI
from dotenv import load_dotenv
from pathlib import Path

# Set up the prompt, brailleR and image

with open('files/breilleR_example2.txt', 'r') as file:
    data = file.read().replace('\n', '')

sysprompt = Path("prompt.txt").read_text()

def encode_image(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode("utf-8")
    
image_path = "files/example2.png"
base64_image = encode_image(image_path)

# Set up the Client -----------------------------

load_dotenv("environment.env")
client = OpenAI(
    api_key= os.getenv("OPENAI_API_KEY")
)

response = client.responses.create(
    model="gpt-4.1",
    instructions="You are a researcher that talk very concisely",
    # input = str(sysprompt) + str(data)
    input=[
        {
            "role": "user",
            "content": [
                { "type": "input_text", "text": "what's in this image?" },
                {
                    "type": "input_image",
                    "image_url": f"data:image/jpeg;base64,{base64_image}",
                },
            ],
        }
    ],
)

print(response.output_text)

