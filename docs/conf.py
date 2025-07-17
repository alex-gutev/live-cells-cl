from os.path import join, dirname, realpath, expandvars

# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'live-cells-cl'
copyright = '2025, Alexander Gutev'
author = 'Alexander Gutev'
release = '0.1.0'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = [
    'sphinx.ext.intersphinx',
    'sphinxcontrib.cldomain',
    'sphinxcontrib.hyperspec'
]

templates_path = ['_templates']
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']



# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'alabaster'
html_static_path = ['_static']


# --- CL domain customizations:
#
# cl_systems: The systems that need to be loaded to make packages
# available documentation
#
# name - The name of the system to load.
# path - The path to the system.
#
# Note: This conf.py sits in a subdirectory below ("../"), relative to where
# the "my-system.asd" system description file lives:

cl_systems = [{"name": "live-cells",
               "path": join(dirname(realpath(__file__)), "../")}]

cl_packages = ["live-cells"]

highlight_language = 'common-lisp'

cl_debug = False
