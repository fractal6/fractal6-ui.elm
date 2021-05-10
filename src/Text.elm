module Text exposing (..)

import Html exposing (Html, span, text)
import String.Extra as SE



-- String


upH : String -> String
upH s =
    SE.toSentenceCase s


upT : String -> String
upT s =
    SE.toTitleCase s


upA : String -> String
upA t =
    String.toUpper t


toText : List String -> Html msg
toText l =
    l
        |> List.intersperse " "
        |> List.map (\x -> text x)
        |> span []


textH : String -> Html msg
textH s =
    s |> upH |> text


textT : String -> Html msg
textT s =
    s |> upT |> text


textA : String -> Html msg
textA s =
    s |> upA |> text



-- General


signin : String
signin =
    "Login"


signout : String
signout =
    "Sign Out"


createAccount : String
createAccount =
    "or create an account"


orSignin : String
orSignin =
    "or sign in"


passwordForgotten : String
passwordForgotten =
    "forgot your password?"


createLabel : String
createLabel =
    "create label"


updateLabel : String
updateLabel =
    "update label"


messageSent : String
messageSent =
    "message sent"


profile : String
profile =
    "profile"


settings : String
settings =
    "settings"


yourOrg : String
yourOrg =
    "organisations"


loading : String
loading =
    "loading"


seeMore : String
seeMore =
    "see more"


welcomIn : String
welcomIn =
    "welcome in"


organisations : String
organisations =
    "organisations"


joinOrga : String
joinOrga =
    "join this organisation"


leaveRole : String
leaveRole =
    "leave role"


checkItOut : String
checkItOut =
    "check it out."


explore : String
explore =
    "explore"


exploreOrganisations : String
exploreOrganisations =
    "explore organisations"


newOrganisation : String
newOrganisation =
    "new organisation"


view : String
view =
    "view"


versions : String
versions =
    "versions"


revisions : String
revisions =
    "revisions"


history : String
history =
    "history"


edit : String
edit =
    "edit"


cancel : String
cancel =
    "cancel"


publish : String
publish =
    "publish"


published : String
published =
    "published"


publishedThe : String
publishedThe =
    "published the"


revisionNotPublished : String
revisionNotPublished =
    "revision not published"


by : String
by =
    "by"


join : String
join =
    "join"


from : String
from =
    "from"


to : String
to =
    "to"


moved : String
moved =
    "moved"


left : String
left =
    "left"


roleLeft : String
roleLeft =
    "role left"


saveChanges : String
saveChanges =
    "save changes"


leaveComment : String
leaveComment =
    "leave a comment"


leaveCommentOpt : String
leaveCommentOpt =
    "Text (optional)"


noResultsFound : String
noResultsFound =
    "no results found"


searchUsers : String
searchUsers =
    "search users"


searchLabels : String
searchLabels =
    "search labels"


editLabels : String
editLabels =
    "edit labels"


reopened : String
reopened =
    "reopened"


closed : String
closed =
    "closed"


assigned : String
assigned =
    "assigned"


unassigned : String
unassigned =
    "unassigned"


archive : String
archive =
    "archive"


unarchive : String
unarchive =
    "unarchive"


archived : String
archived =
    "archived"


unarchived : String
unarchived =
    "unarchived"


documentArchived : String
documentArchived =
    "document archived"


documentUnarchived : String
documentUnarchived =
    "document unarchived"


tensionMoved : String
tensionMoved =
    "tension moved"


leave : String
leave =
    "leave"


the : String
the =
    "the"


updatedTitle : String
updatedTitle =
    "updated title"


confirmUnsaved : String
confirmUnsaved =
    "you have unsaved data, please confirm to exit."


confirmDeleteLabel : String
confirmDeleteLabel =
    "are you sure to delete the label"


askAnotherQuestion : String
askAnotherQuestion =
    "ask another question."


giveAnotherFeedback : String
giveAnotherFeedback =
    "give another feedback."


theOrganisation : String
theOrganisation =
    "the organisation"


moveTension : String
moveTension =
    "move tension"


newContract : String
newContract =
    "new contract"


createContract : String
createContract =
    "create contract"


submit : String
submit =
    "submit"



--Quick Search
--(header)


title : String
title =
    "title"


name : String
name =
    "name"


username : String
username =
    "username"


circle : String
circle =
    "circle"


role : String
role =
    "role"


roles : String
roles =
    "roles"


member : String
member =
    "member"


members : String
members =
    "members"


owner : String
owner =
    "owner"


guest : String
guest =
    "Guest"


parent : String
parent =
    "parent"


firstLink : String
firstLink =
    "first Link"


phQS : String
phQS =
    "find a role or circle"


noResultsFor : String
noResultsFor =
    "no results for"



--Canvas


reverseTooltip : String
reverseTooltip =
    "reverse the organisation graph."


security : String
security =
    "security"



-- Mandate / Members / User


directMembers : String
directMembers =
    "direct members"


subMembers : String
subMembers =
    "sub-circle members"


about : String
about =
    "about"


aboutOpt : String
aboutOpt =
    "about (optional)"


links : String
links =
    "links"


mandate : String
mandate =
    "mandate"


purpose : String
purpose =
    "purpose"


responsabilities : String
responsabilities =
    "accountabilities"


domains : String
domains =
    "domains"


policies : String
policies =
    "policies"


create : String
create =
    "create"


phCirclePurpose : String
phCirclePurpose =
    "Define the purpose of the circle"


phCircleResponsabilities : String
phCircleResponsabilities =
    "Define the circle accountabilities"


phCircleDomains : String
phCircleDomains =
    "Define the circle domains"


phCirclePolicies : String
phCirclePolicies =
    "Define the circle policies"


phRolePurpose : String
phRolePurpose =
    "Define the purpose of the role"


phRoleResponsabilities : String
phRoleResponsabilities =
    "Define the role accountabilities"


phRoleDomains : String
phRoleDomains =
    "Define the role domains"


phRolePolicies : String
phRolePolicies =
    "Define the role policies"


noFirstLinksRole : String
noFirstLinksRole =
    "No members are linked to this role yet."


noFirstLinksCircle : String
noFirstLinksCircle =
    "No members are linked to this circle yet."


addedThe : String
addedThe =
    "added the"


removedThe : String
removedThe =
    "removed the"


description : String
description =
    "description"



-- Tension


conversation : String
conversation =
    "conversation"


document : String
document =
    "document"


contracts : String
contracts =
    "contracts"


status : String
status =
    "status"


type_ : String
type_ =
    "type"


depth : String
depth =
    "depth"


tension : String
tension =
    "tensions"


journal : String
journal =
    "journal"


labels : String
labels =
    "labels"


subLabels : String
subLabels =
    "sub-circle labels"


newLabel : String
newLabel =
    "new label"


assignees : String
assignees =
    "assignees"


action : String
action =
    "action"


noLabels : String
noLabels =
    "none yet"


noAction : String
noAction =
    "no action requested"


noOpenTensionRole : String
noOpenTensionRole =
    "no open tensions for this Role yet."


noOpenTensionCircle : String
noOpenTensionCircle =
    "no open tensions for this Circle yet."


noTensionRole : String
noTensionRole =
    "no tensions for this Role yet."


noTensionCircle : String
noTensionCircle =
    "no tensions for this Circle yet."


internalTensions =
    "internal Tensions"


externalTensions =
    "external Tensions"


noIntTensionRole : String
noIntTensionRole =
    "no internal tensions for this Role yet."


noIntTensionCircle : String
noIntTensionCircle =
    "no internal tensions for this Circle yet."


noExtTensionRole : String
noExtTensionRole =
    "no external tensions for this Role yet."


noExtTensionCircle : String
noExtTensionCircle =
    "no external tensions for this Circle yet."


newTension : String
newTension =
    "new tension"


newCircle : String
newCircle =
    "new circle"


newRole : String
newRole =
    "new role"


editTitle : String
editTitle =
    "edit title"


tensionTitleHelp : String
tensionTitleHelp =
    "title that sumarize your tension."


circleNameHelp : String
circleNameHelp =
    "name of the circle."


roleNameHelp : String
roleNameHelp =
    "name of the role."


orgaNameHelp : String
orgaNameHelp =
    "organisation name."


aboutHelp : String
aboutHelp =
    "short description for this organisation."


purposeHelpOrga : String
purposeHelpOrga =
    "purpose of this organisation."


roleAboutHelp : String
roleAboutHelp =
    "short description for this role."


circleAboutHelp : String
circleAboutHelp =
    "short description for this circle."


tensionMessageHelp : String
tensionMessageHelp =
    "Add a comment to help others understand your issue."


circleMessageHelp : String
circleMessageHelp =
    "add a comment to help others understand why a new circle should be created."


roleMessageHelp : String
roleMessageHelp =
    "add a comment to help others understand why a new role should be created."


firstLinkRoleMessageHelp : String
firstLinkRoleMessageHelp =
    "Select a type of role and assign an user (first link)."


firstLinkCircleMessageHelp : String
firstLinkCircleMessageHelp =
    "Assign an user to a coordinator Role (first link)."


tensionAdded : String
tensionAdded =
    "tension added."


roleAdded : String
roleAdded =
    "role added."


circleAdded : String
circleAdded =
    "circle added."


roleEdited : String
roleEdited =
    "role edited."


circleEdited : String
circleEdited =
    "circle edited."


tensionCircleAdded : String
tensionCircleAdded =
    "Tension added for Circle (not published)."


tensionRoleAdded : String
tensionRoleAdded =
    "Tension added for Role (not published)."


tensionSubmit : String
tensionSubmit =
    "submit tension"


tensionCircleCloseSubmit : String
tensionCircleCloseSubmit =
    "create Circle"


tensionRoleCloseSubmit : String
tensionRoleCloseSubmit =
    "create Role"


openedThe : String
openedThe =
    "opened the"


updatedThe : String
updatedThe =
    "updated the"


editedThe : String
editedThe =
    "edited the"


commentedThe : String
commentedThe =
    "commented the"


updateTitle : String
updateTitle =
    "update title"


updateComment : String
updateComment =
    "update comment"


addResponsabilities : String
addResponsabilities =
    "add accountabilities"


addDomains : String
addDomains =
    "add domains"


addPolicies : String
addPolicies =
    "add policies"



-- Action
--@Todo
-- Organisation


notOrgMember : String
notOrgMember =
    "You are not a member of this organisation."


notCircleMember : String
notCircleMember =
    "You are not a member of this circle."


notCircleCoordo : String
notCircleCoordo =
    "You are not a coordinator of this circle."


askCoordo : String
askCoordo =
    "Please, ask a coordinator of this circle to perform this action."


joinForTension : String
joinForTension =
    "Please, Join this organisation to be able to create a tension."


joinForComment : String
joinForComment =
    "Please, Join this organisation to participate to this conversation."


joinForCircle : String
joinForCircle =
    "Please, Join this organisation to be able to create a circle."


nodeNotExist : String
nodeNotExist =
    "Sorry, this node doesn't exist yet."



--
-- Info
--


contractInfoHeader : String
contractInfoHeader =
    "A peer validation is needed to complete this action."


contractInfo : String
contractInfo =
    "By creating a contract, it will notify the organisation roles with the authority level able to validate your request. The action will be performed once the contract has been validated"
