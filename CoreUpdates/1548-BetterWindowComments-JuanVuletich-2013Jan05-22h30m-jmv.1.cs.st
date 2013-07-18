'From Cuis 4.1 of 12 December 2012 [latest update: #1544] on 5 January 2013 at 10:40:22 pm'!

!SystemWindow commentStamp: '<historical>' prior: 0!
SystemWindow is the Morphic implementation of Window: a labelled container for rectangular views, with iconic facilities for close, collapse/expand, and resizing.

The attribute onlyActiveOnTop, if set to true (and any call to activate will set this), determines that only the top member of a collection of such windows on the screen shall be active.  To be not active means that a mouse click in any region will only result in bringing the window to the top and then making it active.!


!CodePackageListWindow commentStamp: '<historical>' prior: 0!
Morphic view for CodePackageList models. See category 'GUI building'.!


!CodeWindow commentStamp: '<historical>' prior: 0!
A hierarchy of morphic views, parallel to the CodeProvider models hierarchy. See category 'GUI building'.!


!BrowserWindow commentStamp: '<historical>' prior: 0!
A morphic window for Browser models. See category 'GUI building'.

(BrowserWindow new model: Browser new; buildMorphicWindow) openInWorld

instead of

Browser openBrowser


----
No dejar en clases de modelo comportamiento que se haya implementado aca!!!!!!!!
el unico protocolo repetido deberia ser delegacion!


!ChangeListWindow commentStamp: '<historical>' prior: 0!
Morphic view for ChangeList models. See category 'GUI building'.!


!ChangeSorterWindow commentStamp: '<historical>' prior: 0!
Morphic view for ChangeSorter models. See category 'GUI building'.!


!CodeFileBrowserWindow commentStamp: '<historical>' prior: 0!
Morphic view for CodeFileBrowser models. See category 'GUI building'.!


!DebuggerWindow commentStamp: '<historical>' prior: 0!
Morphic view for Debugger models. See category 'GUI building'.!


!FileListWindow commentStamp: '<historical>' prior: 0!
Morphic view for FileList models. See category 'GUI building'.!


!HierarchyBrowserWindow commentStamp: '<historical>' prior: 0!
Morphic view for HierarchyBrowser models. See category 'GUI building'.!


!MessageSetWindow commentStamp: '<historical>' prior: 0!
Morphic view for MessageSet models. See category 'GUI building'.!


!MessageNamesWindow commentStamp: '<historical>' prior: 0!
Morphic view for MessageNames models. See category 'GUI building'.!


!ObjectExplorerWindow commentStamp: '<historical>' prior: 0!
Morphic view for ObjectExplorer models. See category 'GUI building'.!


!PreDebugWindow commentStamp: '<historical>' prior: 0!
Morphic view for Debugger models. See category 'GUI building'.!


!ProcessBrowserWindow commentStamp: '<historical>' prior: 0!
Morphic view for ProcessBrowser models. See category 'GUI building'.!


!RecentMessageSetWindow commentStamp: '<historical>' prior: 0!
Morphic view for RecentMessageSet models. See category 'GUI building'.!


!TestRunnerWindow commentStamp: '<historical>' prior: 0!
Morphic view for TestRunner models. See category 'GUI building'.!


!TranscriptWindow commentStamp: '<historical>' prior: 0!
Morphic view for Transcript. See category 'GUI building'.!


!VersionsBrowserWindow commentStamp: '<historical>' prior: 0!
Morphic view for VersionsBrowser models. See category 'GUI building'.!


!WorkspaceWindow commentStamp: '<historical>' prior: 0!
Morphic view for Workspace models. See category 'GUI building'.!

