# The Cuis Smalltalk Release Process

This document describes the Process to handle Stable Releases for Cuis. It is inspired by RedHat Linux. The latest Stable Releases is at the [Cuis 6.2 repo](https://github.com/Cuis-Smalltalk/Cuis6-2). Alpha development is done at the [Rolling Release of Cuis](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev).

The process described here produces two releases per year: One in May and another November. 

## TL;DR (Short version) 

For instance, if goal is to do release of Cuis 7.0 on May 31st. 

On date March 31st (D - 2 months), new development in the main Cuis-Smalltalk-Dev repo is ended, to focus on getting ready for the creation of Cuis 7.0 "release candidate". This is the Beta phase, it lasts 1 month. 

On date April 30th (D - 1 month), we create a repo for Cuis 7.0 "release candidate", and focus on QA and final tweaks on this release specific repo. This phase also lasts 1 month. 

On date May 31st we do release of Cuis 7.0 on the repo created above. 

After release, any critical fixes are pushed to the Cuis 7.0 repo. 

## Detailed version

The following process will be repeated for each release. Currently the target is to do two releases per year, meaning 6 month per release. These are split in: 

Four months of unencumbered ("Aplha") development. 

One month of "Beta" testing and addressing remaining and newfound issues. 

One month of "Release Candidate", with focus on Documentation and QA, and any newfound issue. Shouldn't be many. 

In general, all the activities are done all the time. We don't need to wait for "Beta" to fix issues, or to "RC" to do careful QA. The restriction is that during "Beta" we add no new functionality, and refrain from risky changes. Additionally, during "RC" we only accept fixes. But those kinds of developments are not stopped, it is just that they won't be integrated into the main repo, and will need to stay in a separate branch.

Let's go through a detailed example of what we'll do for each release. We start with the Cuis main development repository on GitHub, i.e. the "Rolling Release". URL is https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev 

Let's assume the next release will be Cuis version 7.0, and will be released on May 31, 2024. This example timeline starts six months before that.

#### November 1, 2023 to March 31, 2024 - "Alpha development in Cuis-Smalltalk-Dev"
**Cuis-Smalltalk-Dev repository**
* Regular Alpha development, "Rolling Release" style. Four months of unconstrained development, with focus on new features and possibly deep improvements for existing ones. Also bug fixing. Quick integration of updates.
* Any maintenance fixes to already released versions of Cuis are considered for integration here.

#### April 1, 2024 to April 29, 2024 - "Beta phase in Cuis-Smalltalk-Dev in preparation for Cuis 7.0"
**Cuis-Smalltalk-Dev repository**
* Beta phase. Development of new features slows down. High risk ones are postponed. If needed, a decision is made to leave unfinished features out of Cuis 7.0. Focus is on finishing, polishing and doing QA on the features that will be part of Cuis 7.0, including selected additional packages.
* Any maintenance fixes to already released versions of Cuis are considered for integration here.

#### April 30, 2024 - "Creation of the Cuis 7.0 Release Candidate"
**Cuis-Smalltalk-Dev repository**
* The Cuis-Smalltalk-Dev repo is in a very good shape for the planned release: Cuis 7.0. Required features are fully implemented and reasonably tested.
* Tag the repo: **`git tag BaseForCuis7.0`** then **`git push origin BaseForCuis7.0`**
* Build the Release version **`World / Save Official Release`** then **`World / Save Updated and Quit`**
* Create the new Cuis-7 repository. Add files as required. Selected additional code packages from other https://github.com/Cuis-Smalltalk repositories are added.

#### May 1, 2024 to May 30, 2024 - "QA and polishing of the Cuis 7.0 Release Candidate"
**Cuis-7 repository**
* During this month, Cuis-7 is in the "Release Candidate" phase. Final QA is done. Any problems found are fixed. An independent update stream of changes is maintained for this repo, including only fixes for Cuis 7.0 and the included packages.
* Any maintenance fixes to already released versions of Cuis are considered for integration here

**Cuis-Smalltalk-Dev repository**
* In this phase, it is expected that few new changes are integrated in the main Cuis repository, as most of the focus is on the Cuis-7 repository. Still there may be some. They will not get included in the Cuis-7 repo. Any fixes done in the Cuis-7 repo are integrated here, though. Separate teams may start working on features for Cuis 7.1, but that work is most likely not integrated during this phase.
* Any maintenance fixes to already released versions of Cuis are considered for integration here.

#### May 31, 2024 - "Official release of Cuis 7.0"
**Cuis-7 repository**
* The final version of the Cuis 7.0 image and selected packages is generated and published at the repo. Announcements are made.

#### May 31, 2024 onwards - "Maintenance phase of Cuis 7.0"
**Cuis-7 repository**
* Any fixes for serious bugs found after release are published as updates. No new features are added. This phase could last several years, but activity should be very low.
* If applicable, fixes produced here are integrated into Cuis-Smalltalk-Dev, and possibly into other releases that are in "Maintanance phase".
* Possible maintenance fixes to other released versions of Cuis are considered for integration.

#### June 1, 2024 to September 30, 2024 - "Alpha development in Cuis-Smalltalk-Dev"
**Cuis-Smalltalk-Dev repository**
* A new 6 month cycle starts here.
* Cuis Rolling Release version is bumped to 7.1.
