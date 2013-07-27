StyledTextEditor
----------------

This is the StyledTextEditor project for Cuis, brought to you by Bernhard Pieber and Juan Vuletich. The design of the Styled Text Editor toolbar and scrollbars was done by Sabine Gasper-Mautes.

### Installation ###

Steps to install:
If you want to install StyledTextEditor in a Cuis 4.2 image for the first time do the following steps:
- Start Cuis 4.2, preferably with a VM that includes support for the ExtendedClipboardPlugin (like this: http://www.squeakvm.org/mac/release/Squeak%204.2.5beta1U.zip , currently only on the Mac)
- Open… > File List
- Navigate to subdirectory Packages/StyledTextEditor
- Select StyledTextInstaller.pck.st
- Click the toolbar button 'install package'

Steps to update:
If you already installed StyledTextEditor in an image you can update it to a newer version like this:
- Open… > Installed Packages
- Save unsaved packages if necessary so that Git can merge your changes
- Pull from the GitHub repository of Cuis-StyledTextEditor you are using
- Do the following: StyledTextInstaller new update


### Features ###

The Styled Text Editor is a framework for rich text editing using styles as known from popular word processors like Apple Pages or Microsoft Word. It features paragraph and character styles, allowing easy text formatting using styles only. It is intended for applications where users need to work with good looking rich text in a simple and fast way.

Rich text commonly refers to text with formatting information like different fonts, sizes, alignments, and emphasis. To make editing as simple as possible instead of individual formatting information end users apply styles to parts of the text. Editing is made fast by keyboard shortcuts for text navigation, selection, and styles selection. Where possible the feel of widely used rich text editors is supported.

The editor includes features like
- numbered and bulleted list paragraph styles
- support for images
- multiple level undo and redo
- text completion using various glossaries including a dictionary of English with about 166.000 words
- RTF clipboard (on OS X VMs with the ClipboardExtendedPlugin)

The Styled Text Editor is developed in Cuis with the plan to eventually port it to Squeak and Pharo. The development of the Styled Text Editor inspired many changes to Cuis itself, and It is the first package to use the brand new DVCS based development process for external packages for Cuis 4.0. 

### Acknowlegements ###

The idea and funding was provided Bernhard Pieber and his company Software Generation. The implementation was done by Juan Vuletich, the mastermind behind Cuis. Thanks Juan for the close cooperation. It was and still is great fun to work with you.

Bernhard and Juan are looking forward to feedback from you. Fork it, create issues and send pull requests. ;-)


### References ###

[1] http://www.esug.org/wiki/pier/Conferences/2011/Schedule-And-Talks/StyledTextEditor

[2] http://www.youtube.com/watch?v=pUoVbvwspi8&list=PL813665D04A2E4C0A&index=7&feature=plpp_video
