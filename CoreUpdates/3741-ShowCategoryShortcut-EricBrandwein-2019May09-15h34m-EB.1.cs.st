'From Cuis 5.0 of 7 November 2016 [latest update: #3729] on 9 May 2019 at 4:02:29 pm'!

!CodeWindow methodsFor: 'keyboard shortcuts' stamp: 'EB 5/9/2019 16:00:27'!
messageListKey: aChar from: view
	"Respond to a Command key.  I am a model with a code pane, and I also
	have a listView that has a list of methods.  The view knows how to get
	the list and selection."

	| sel class |
	aChar == $D ifTrue: [^ model toggleDiffing].

	sel _ model selectedMessageName.
	aChar == $m ifTrue: [  "These next two put up a type in if no message selected"
		^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllImplementorsOf: to: Smalltalk].
	aChar == $n ifTrue: [
		^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllCallsOn: to: Smalltalk].

	"The following require a class selection"
	(class _ model selectedClassOrMetaClass) ifNil: [^ self ].
	aChar == $b ifTrue: [^ BrowserWindow fullOnClass: class selector: sel].
	aChar == $N ifTrue: [^ self browseClassRefs].
	aChar == $i ifTrue: [^ self methodHierarchy].
	aChar == $h ifTrue: [^ self browseHierarchy].
	aChar == $p ifTrue: [^ self browseFullProtocol].

	sel
		ifNotNil: [
			"The following require a method selection"
			aChar == $o ifTrue: [^ model fileOutMessage].
			aChar == $c ifTrue: [^ model copySelector].
			aChar == $v ifTrue: [^ self browseVersions].
			aChar == $C ifTrue: [^ model showHomeCategory].
			aChar == $O ifTrue: [^ self openSingleMessageBrowser].
			aChar == $x ifTrue: [^ model removeMessage].
			aChar == $t ifTrue: [^ model runMethodTest].
			aChar == $r ifTrue: [^ model debugMethodTest].
			aChar == $R ifTrue: [^ self renameSelector]]
		ifNil: [
			aChar == $R ifTrue: [^ model renameClass]]! !

