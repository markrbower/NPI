# Analysis Directory Overview 

The Analysis directory contains folders for different types of code. Keeping all code in one subdirectory collects things for easier transfer and also makes version control easier.

Each language has its own directory, allowing language-specific entities to be kept with code (e.g., a makefile for C, a project name for R, AspectJ files for Java, CSS stylesheets for JavaScript, etc.). Any Concept that has this directory structure is assumed to not be under GitHub version control.

By creating a Git repository in this directory, all code folders (and thus all code) can be put under a single, remote repository. The following steps do just that:

	1.	On GitHub, create a new project for this Concept (referred to as <your_GitHub_project_name>)
	2.	Go to the Analysis directory
	3.	Move all code folders into a single folder
	⁃	mkdir safe; mv !(safe) safe
	⁃	This assumes that you have ‘shopt -s extglob’ in your bashrc file.
	⁃	Otherwise, run ‘mv * safe 2>/dev/null’ to suppress the warning about moving ‘safe’ into itself
	4.	git clone git@github.com:<your_GitHub_username>/<your_GitHub_project_name>.git
	⁃	example: git clone git@github.com:markrbower/NPO.git
	5.	mv safe/* <your_GitHub_project_name>
	6.	git add .
	7.	git commit -m “Initial commit”
	8.	git push

For R, in particular, the RStudio project file (template.Rproj) is in the Analysis directory at the same level as the R directory. This is intentional; following the steps above will cause that project file to be contained in the <your_GitHub_project_name> folder, which will cause it to be correctly named when opened in RStudio (RStudio displays the name of the containing folder).






