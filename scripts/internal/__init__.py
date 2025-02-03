import os

# Get the path of the script
script_path = os.path.realpath(__file__)
script_dir = os.path.dirname(script_path)

# Set the path to the templates directory
templ_path = os.path.join(script_dir, "../templates/48r1")
