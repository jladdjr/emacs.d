#!/bin/bash

git push
git co org_roam_issue_2384
git rebase main
git push --force-with-lease
git co main
