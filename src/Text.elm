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


space_ : String
space_ =
    "\u{00A0}"



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


createRole : String
createRole =
    "create role"


updateRole : String
updateRole =
    "update role"


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


revisionNotPublished : String
revisionNotPublished =
    "revision not published"


by : String
by =
    "by"


joined : String
joined =
    "joined"


created : String
created =
    "created"


from : String
from =
    "from"


to : String
to =
    "to"


moved : String
moved =
    "moved"


changed : String
changed =
    "changed"


move : String
move =
    "move"


visibility : String
visibility =
    "visibility"


governance : String
governance =
    "governance"


authority : String
authority =
    "authority"


linkAction : String
linkAction =
    "Invite a first-link for the role"


unlinkAction : String
unlinkAction =
    "Unlink the first-link for the role"


invite : String
invite =
    "invite"


link : String
link =
    "link"


unlink : String
unlink =
    "unlink"


linked : String
linked =
    "linked"


unlinked : String
unlinked =
    "unlinked"


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


this : String
this =
    "this"


confirmUnsaved : String
confirmUnsaved =
    "you have unsaved data, please confirm to exit."


confirmUnsafe : String
confirmUnsafe =
    "you have unsaved data, please confirm to continue (your editing will be lost)."


confirmDeleteLabel : String
confirmDeleteLabel =
    "are you sure to delete the label"


confirmDeleteRole : String
confirmDeleteRole =
    "are you sure to delete the role"


confirmDeleteContract : String
confirmDeleteContract =
    "are you sure to cancel this contract"


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


moveNode : String
moveNode =
    "move node"


newContract : String
newContract =
    "new contract"


contractType : String
contractType =
    "contract type"


contractEvent : String
contractEvent =
    "event"


createContract : String
createContract =
    "create contract"


submit : String
submit =
    "submit"


add : String
add =
    "add"


remove : String
remove =
    "remove"


delete : String
delete =
    "delete"


subscribe : String
subscribe =
    "subscribe"


unsubscribe : String
unsubscribe =
    "unsubscribe"


notifications : String
notifications =
    "notifications"


tensionSubscribeText : String
tensionSubscribeText =
    "You’re receiving notifications because you’re subscribed to this thread."


tensionUnsubscribeText : String
tensionUnsubscribeText =
    "You’re not receiving notifications from this thread. "



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


nodeNotFound : String
nodeNotFound =
    "Node is archived, hidden or has moved."



--Canvas


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


hasBeen : String
hasBeen =
    "has been"


toThisRole : String
toThisRole =
    "to this role"


color : String
color =
    "color"



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


noneYet : String
noneYet =
    "none yet"


noDocument : String
noDocument =
    "no document attached"


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


internalTensions : String
internalTensions =
    "internal Tensions"


externalTensions : String
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


opened : String
opened =
    "opened"


updated : String
updated =
    "updated"


edited : String
edited =
    "edited"


commented : String
commented =
    "commented"


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


goRoot : String
goRoot =
    "Go to root circle"


goParent : String
goParent =
    "Go to parent circle"


clickMe : String
clickMe =
    "Click me"


newReceiver : String
newReceiver =
    "new destination"


noLabels : String
noLabels =
    "No label here yet"


noRoles : String
noRoles =
    "No role here yet"


noLabelsSub : String
noLabelsSub =
    "No new sub-circle label yet"


noLabelsTop : String
noLabelsTop =
    "No new parent label yet"


noRolesSub : String
noRolesSub =
    "No new sub-circle role yet"


noRolesTop : String
noRolesTop =
    "No new parent role yet"


labelsSub : String
labelsSub =
    "Labels in sub-circles"


labelsTop : String
labelsTop =
    "Labels in parent circles"


rolesSub : String
rolesSub =
    "Roles in sub-circles"


rolesTop : String
rolesTop =
    "Roles in parent circles"



--
-- Tooltip
--


reverseTooltip : String
reverseTooltip =
    "Reverse the organisation graph."


tensionsListTooltip : String
tensionsListTooltip =
    "Display tensions as List."


tensionsIntExtTooltip : String
tensionsIntExtTooltip =
    "Display indegenous and exogenous tensions with respect to the focused circle."


tensionsCircleTooltip : String
tensionsCircleTooltip =
    "Display tensions by (targeted) circles."



--
-- Info / Doc
--


labelDeleteInfoHeader : String
labelDeleteInfoHeader =
    "Label present in other circles, and in labeled tensions, wont' be deleted."


roleDeleteInfoHeader : String
roleDeleteInfoHeader =
    "Role present in other circles, and created roles, wont' be deleted."


contractInfoHeader : String
contractInfoHeader =
    "A peer validation is needed to complete this action."


contractInfo : String
contractInfo =
    "By creating a contract, it will notify the organisation roles with the authority level able to validate your request. The action will be performed once the contract has been validated"


tensionTypeHeader : String
tensionTypeHeader =
    "Select the right category to help finding and solving tensions."


tensionTypeDoc : String
tensionTypeDoc =
    """ There is three main categories of tension:
* **Operational**: A general category of tension which can be a task, an issue or whatever that need to be share in the organisation.
* **Governance**: concerning the structure of the organisation its mandates etc.
* **Help**: for help requests, questions or clarifications.
    """


visibilityInfoHeader : String
visibilityInfoHeader =
    "The change will only apply on the current circle and its roles, not recursively on the sub-circles."


visibilityPublic : String
visibilityPublic =
    "The entire world can see it."


visibilityPrivate : String
visibilityPrivate =
    "Only member of the organisation can see it."


visibilitySeccret : String
visibilitySeccret =
    "Only roles below this circle can see it."


circleAuthorityHeader : String
circleAuthorityHeader =
    "The governance authority is characterized by the right to do a set of actions."


circleAuthorityDoc : String
circleAuthorityDoc =
    """
* Create and edit roles and circles
* Edit tensions (Title, assignees etc)
* Edit mandates
* invite new members

Note: To edit mandates you need to publish the modifications done in the corresponding (governance) tension.
    """


authCoordinated : String
authCoordinated =
    "Where Coordinator roles (orange) have the governance authority."


authAgile : String
authAgile =
    "Where all roles have equal governance authority."


labelsInfoHeader : String
labelsInfoHeader =
    "Creating new labels help you to sort out your tensions"


labelsInfoDoc : String
labelsInfoDoc =
    """
  When you create a label here, you can then attach it to your tensions. You will also be able to filter the tension with a labels selection.

  **Scope**: The labels that you can attach to a tension are those who are present in either its receicer circle or in any of the parents of that receiver circle.

  """


rolesInfoHeader : String
rolesInfoHeader =
    "Creating new roles help you to manage your organization"


rolesInfoDoc : String
rolesInfoDoc =
    """
  When you create a role here, you can use it as a template when you create a new role in your organisation.

  **Scope**: The roles template that you can use when creating a new role are those who are present in any of its parents.

  """
