'From Cuis 5.0 [latest update: #3852] on 1 September 2019 at 2:19:12 pm'!

!Utilities class methodsFor: 'default desktop' stamp: 'pb 9/1/2019 14:07:42'!
pvtDefaultTextEditorContents
	^ Text
		string: '

Cuis Smalltalk

 

"Yay, Juan.  You GO, guy!! ...a great example of malleable software (and a clever mind) at work."
 Dan Ingalls

"I like it... It''s nice and clean and simple and pretty. Nice stuff!!"
 Alan Kay
 
"I think you have a very elegant design aesthetic."
 John Maloney
 
Cuis is:

    - Small
    - Clean
    - Appropriable


Like Squeak, Cuis is also:

    - Open Source
    - Multiplatform

 
Like other Smalltalk systems (including Squeak, Pharo and others), Cuis is also:

    - A complete development environment written in itself
    - A pure, dynamic Object Oriented language


Cuis assumes very little on the underlying platform, and this lets it run out-of-the-box on Windows, MacOS, Linux, ChromeOS and WebBrowsers. Cuis shares the [OpenSmalltalk Virtual Machine](http://www.opensmalltalk.org) with Squeak, Pharo and Newspeak.

What sets Cuis apart from the other members of the Squeak family is the focus on Smalltalk-80 and an active attitude towards system complexity:

Unbound complexity growth, together with development strategies focused only in the short term, are the worst long term enemies of all software systems. As systems grow older, they usually become more complex. New features are added as layers on top of whatever is below, sometimes without really understanding it, and almost always without modifying it. Complexity and size grow without control. Evolution slows down. Understanding the system becomes harder every day. Bugs are harder to fix. Codebases become huge for no clear reason. At some point, the system can''t evolve anymore and becomes "legacy code".

Complexity puts a limit to the level of understanding of the system a person might reach, and therefore limits the things that can be done with it. Dan Ingalls says all this in ["Design Principles Behind Smalltalk"](http://www.cs.virginia.edu/~evans/cs655/readings/smalltalk.html). Even if you have already done so, please go and read it again!!

Cuis Smalltalk is our attempt at this challenge. Furthermore, we believe we are doing something else that no other Smalltalk, commercial or open source, does. We attempt to give the true Smalltalk-80 experience, and keep Smalltalk-80 not as legacy software historic significance, but as a live, evolving system. We feel we are the keepers of the Smalltalk-80 heritage, and enablers of the Dynabook experience.

Cuis is continuously evolving towards simplicity. Each release is better (i.e. simpler) than the previous one. At the same time, features are enhanced, and any reported bugs fixed. We also adopt recent enhancements from Squeak and share our work with the wider Squeak and Smalltalk community.


License

Cuis is distributed subject to the MIT License, as in http://www.opensource.org/licenses/mit-license.php . Any contribution submitted for incorporation into or for distribution with Cuis shall be presumed subject to the same license.
Portions of Cuis are:
Copyright (c) Xerox Corp. 1981, 1982
Copyright (c) Apple Computer, Inc. 1985-1996
Copyright (c) Contributors to Squeak and Cuis projects. 1997-2019'
		runs:
			(RunArray
				runs: #(2 14 1 4 98 11 73 8 56 12 1 1 1 8 2 42 3 26 2 37 4 80 2 105 2065 7 405 )
				values:
					((Array new: 27)
						
							at: 1
							put: #();
						
							at: 2
							put:
								((Array new: 4)
									
										at: 1
										put:
											(TextAlignment basicNew
												
													instVarAt: 1
													put: 2;
												yourself);
									
										at: 2
										put:
											(TextFontFamilyAndSize basicNew
												
													instVarAt: 1
													put: 'DejaVu';
												
													instVarAt: 2
													put: 22;
												yourself);
									
										at: 3
										put:
											(TextEmphasis basicNew
												
													instVarAt: 1
													put: 1;
												yourself);
									
										at: 4
										put:
											(TextColor basicNew
												
													instVarAt: 1
													put:
														(Color
															r: 0.0
															g: 0.0
															b: 1.0);
												yourself);
									yourself);
						
							at: 3
							put:
								((Array new: 1)
									
										at: 1
										put:
											(TextAlignment basicNew
												
													instVarAt: 1
													put: 2;
												yourself);
									yourself);
						
							at: 4
							put: #();
						
							at: 5
							put:
								((Array new: 2)
									
										at: 1
										put:
											(TextAlignment basicNew
												
													instVarAt: 1
													put: 1;
												yourself);
									
										at: 2
										put:
											(TextFontFamilyAndSize basicNew
												
													instVarAt: 1
													put: 'DejaVu';
												
													instVarAt: 2
													put: 9;
												yourself);
									yourself);
						
							at: 6
							put:
								((Array new: 3)
									
										at: 1
										put:
											(TextAlignment basicNew
												
													instVarAt: 1
													put: 1;
												yourself);
									
										at: 2
										put:
											(TextEmphasis basicNew
												
													instVarAt: 1
													put: 2;
												yourself);
									
										at: 3
										put:
											(TextFontFamilyAndSize basicNew
												
													instVarAt: 1
													put: 'DejaVu';
												
													instVarAt: 2
													put: 9;
												yourself);
									yourself);
						
							at: 7
							put:
								((Array new: 2)
									
										at: 1
										put:
											(TextAlignment basicNew
												
													instVarAt: 1
													put: 1;
												yourself);
									
										at: 2
										put:
											(TextFontFamilyAndSize basicNew
												
													instVarAt: 1
													put: 'DejaVu';
												
													instVarAt: 2
													put: 9;
												yourself);
									yourself);
						
							at: 8
							put:
								((Array new: 3)
									
										at: 1
										put:
											(TextAlignment basicNew
												
													instVarAt: 1
													put: 1;
												yourself);
									
										at: 2
										put:
											(TextEmphasis basicNew
												
													instVarAt: 1
													put: 2;
												yourself);
									
										at: 3
										put:
											(TextFontFamilyAndSize basicNew
												
													instVarAt: 1
													put: 'DejaVu';
												
													instVarAt: 2
													put: 9;
												yourself);
									yourself);
						
							at: 9
							put:
								((Array new: 2)
									
										at: 1
										put:
											(TextAlignment basicNew
												
													instVarAt: 1
													put: 1;
												yourself);
									
										at: 2
										put:
											(TextFontFamilyAndSize basicNew
												
													instVarAt: 1
													put: 'DejaVu';
												
													instVarAt: 2
													put: 9;
												yourself);
									yourself);
						
							at: 10
							put:
								((Array new: 3)
									
										at: 1
										put:
											(TextAlignment basicNew
												
													instVarAt: 1
													put: 1;
												yourself);
									
										at: 2
										put:
											(TextEmphasis basicNew
												
													instVarAt: 1
													put: 2;
												yourself);
									
										at: 3
										put:
											(TextFontFamilyAndSize basicNew
												
													instVarAt: 1
													put: 'DejaVu';
												
													instVarAt: 2
													put: 9;
												yourself);
									yourself);
						
							at: 11
							put:
								((Array new: 2)
									
										at: 1
										put:
											(TextAlignment basicNew
												
													instVarAt: 1
													put: 1;
												yourself);
									
										at: 2
										put:
											(TextFontFamilyAndSize basicNew
												
													instVarAt: 1
													put: 'DejaVu';
												
													instVarAt: 2
													put: 9;
												yourself);
									yourself);
						
							at: 12
							put:
								((Array new: 1)
									
										at: 1
										put:
											(TextFontFamilyAndSize basicNew
												
													instVarAt: 1
													put: 'DejaVu';
												
													instVarAt: 2
													put: 9;
												yourself);
									yourself);
						
							at: 13
							put: #();
						
							at: 14
							put:
								((Array new: 1)
									
										at: 1
										put:
											(TextEmphasis basicNew
												
													instVarAt: 1
													put: 1;
												yourself);
									yourself);
						
							at: 15
							put: #();
						
							at: 16
							put:
								((Array new: 2)
									
										at: 1
										put:
											(TextColor basicNew
												
													instVarAt: 1
													put:
														(Color
															r: 0.0
															g: 0.0
															b: 1.0);
												yourself);
									
										at: 2
										put:
											(TextFontFamilyAndSize basicNew
												
													instVarAt: 1
													put: 'DejaVu';
												
													instVarAt: 2
													put: 14;
												yourself);
									yourself);
						
							at: 17
							put: #();
						
							at: 18
							put:
								((Array new: 1)
									
										at: 1
										put:
											(TextEmphasis basicNew
												
													instVarAt: 1
													put: 1;
												yourself);
									yourself);
						
							at: 19
							put: #();
						
							at: 20
							put:
								((Array new: 2)
									
										at: 1
										put:
											(TextFontFamilyAndSize basicNew
												
													instVarAt: 1
													put: 'DejaVu';
												
													instVarAt: 2
													put: 14;
												yourself);
									
										at: 2
										put:
											(TextColor basicNew
												
													instVarAt: 1
													put:
														(Color
															r: 0.0
															g: 0.0
															b: 1.0);
												yourself);
									yourself);
						
							at: 21
							put: #();
						
							at: 22
							put:
								((Array new: 1)
									
										at: 1
										put:
											(TextEmphasis basicNew
												
													instVarAt: 1
													put: 1;
												yourself);
									yourself);
						
							at: 23
							put: #();
						
							at: 24
							put:
								((Array new: 2)
									
										at: 1
										put:
											(TextColor basicNew
												
													instVarAt: 1
													put:
														(Color
															r: 0.0
															g: 0.0
															b: 1.0);
												yourself);
									
										at: 2
										put:
											(TextFontFamilyAndSize basicNew
												
													instVarAt: 1
													put: 'DejaVu';
												
													instVarAt: 2
													put: 14;
												yourself);
									yourself);
						
							at: 25
							put: #();
						
							at: 26
							put:
								((Array new: 1)
									
										at: 1
										put:
											(TextEmphasis basicNew
												
													instVarAt: 1
													put: 1;
												yourself);
									yourself);
						
							at: 27
							put: #();
						yourself)).! !

!Utilities class methodsFor: 'default desktop' stamp: 'pb 9/1/2019 13:55:38'!
recreateDefaultDesktop
	| editor |
	TranscriptWindow openTranscript
		morphPosition: 5 @ 290;
		morphExtent: 988 @ 399.
	editor _ TextEditor openTextEditor
		morphPosition: 463 @ 4;
		morphExtent: 894 @ 686.
	editor model actualContents: self pvtDefaultTextEditorContents.
	self runningWorld showTaskbar.! !

!Utilities class methodsFor: 'default desktop' stamp: 'pb 9/1/2019 14:18:37'!
tearDownDesktop
	self runningWorld hideTaskbar.
	SystemWindow allSubInstancesDo: [ :ea |
		ea delete ].! !


!TextEditor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 13:30:38'!
openTextEditor

	^ SystemWindow editText: TextModel new label: 'Text Editor' wrap: true! !


!TranscriptWindow class methodsFor: 'GUI building' stamp: 'pb 9/1/2019 13:27:51'!
openTranscript
	"
	TranscriptWindow openTranscript
	"
	| win |
	win _ TranscriptWindow new.
	win
		setLabel: 'Transcript';
		model: Transcript.
	win layoutMorph addMorph: TranscriptMorph new proportionalHeight: 1.
	win model when: #redraw send: #redrawNeeded to: win.
	^ win openInWorld.
! !


!Utilities class reorganize!
('cached state access' releaseClassCachedState releaseClassState)
('common requests' saveDisplay saveScreenshot)
('default desktop' pvtDefaultTextEditorContents recreateDefaultDesktop tearDownDefaultDesktop)
('identification' authorInitials authorInitialsPerSe authorName changeStamp clearAuthor dateTimeSuffix fixStamp: logsUserChanges logsUserChanges: methodsWithInitials: setAuthor)
('miscellaneous' awaitMouseUpIn:repeating:ifSucceed: awaitMouseUpIn:whileMouseDownDo:whileMouseDownInsideDo:ifSucceed: garbageCollectAndReport)
('summer97 additions' browseVersionsForClass:selector: classFromPattern:withCaption: hierarchyOfClassesSurrounding: hierarchyOfImplementorsOf:forClass:)
('support windows' codeManagementInCuisContents cuisAndGitHubContents openCodeManagementInCuis openCuisAndGitHub openUsefulExpressions usefulExpressionsContents)
('tailoring system' stripMethods:messageCode:)
('user interface' informUser:during: informUserDuring:)
('vm statistics' reportCPUandRAM textMarkerForShortReport vmStatisticsReportString vmStatisticsShortString)
!

