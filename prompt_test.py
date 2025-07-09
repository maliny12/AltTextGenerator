import os
from openai import OpenAI
from dotenv import load_dotenv

with open('files/breilleR_example1.txt', 'r') as file:
    data = file.read().replace('\n', '')

load_dotenv("environment.env")
client = OpenAI(
    api_key= os.getenv("OPENAI_API_KEY")
)

response = client.responses.create(
    model = "gpt-4.1",
    instructions="You are a researcher that talk very concisely. Describe what you can learn from the plot",
    input = data
)

print(response.output_text)

