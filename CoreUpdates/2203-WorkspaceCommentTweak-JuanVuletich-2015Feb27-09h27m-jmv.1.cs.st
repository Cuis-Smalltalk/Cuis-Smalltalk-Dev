'From Cuis 4.2 of 25 July 2013 [latest update: #2202] on 27 February 2015 at 9:29:36.794748 am'!

!Workspace commentStamp: '<historical>' prior: 0!
A Workspace is a text area plus a lot of support for executable code.  It is a great place to execute top-level commands to compute something useful, and it is a great place to develop bits of a program before those bits get put into class methods.

To open a new workspace, execute:

	Workspace openLabel: 'a workspace'


A workspace can have its own variables, called "workspace variables", to hold intermediate results.  For example, if you type into a workspace "x := 5" and do-it, then later you could type in "y := x * 2" and y would become 10.

Additionally, in Morphic, a workspace can gain access to morphs that are on the screen.  If acceptDroppedMorphss is turned on, then whenever a morph is dropped on the workspace, a variable will be created which references that morph.  This functionality is toggled with the window-wide menu of a workspace.!

