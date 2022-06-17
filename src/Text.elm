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


createYourAccount : String
createYourAccount =
    "Create your account"


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


confirm : String
confirm =
    "confirm"


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


join : String
join =
    "join"


inviteMember : String
inviteMember =
    "Invite member"


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


back : String
back =
    "back"


almostThere : String
almostThere =
    "almost there"


sort : String
sort =
    "sort"


new : String
new =
    "new"



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


label : String
label =
    "label"


accept : String
accept =
    "accept"


decline : String
decline =
    "decline"



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


invitationMessageHelp : String
invitationMessageHelp =
    "Add an invitation message."


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
    "Please, Join this organisation to be able to create a tension (or retry)."


joinForComment : String
joinForComment =
    "Please, Join this organisation to participate to this conversation."


joinForCircle : String
joinForCircle =
    "Please, Join this organisation to be able to create a circle (or retry)."


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
    "No label in this circle yet"


noRoles : String
noRoles =
    "No role in this circle yet"


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


noDataError : String
noDataError =
    "No data returned. You may try to refresh the page."



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
    """ There are three main categories of tension:
* **Operational**: A general category, it can be task, an issue or whatever that need to be share in the organisation.
* **Governance**: Concerning the structure of the organisation, the mandates etc.
* **Help**: For help requests, questions or clarifications.
* **Alert**: For alert requests, this are used for global annoncement. All the member behind the circle alerted will be notified (Coordinator level to trigger).
    """


visibilityInfoHeader : String
visibilityInfoHeader =
    "The change will only apply on the current circle and its roles, not recursively on the sub-circles."


visibilityPublic : String
visibilityPublic =
    "The entire world can view."


visibilityPrivate : String
visibilityPrivate =
    "Only members of the organisation can view."


visibilitySeccret : String
visibilitySeccret =
    "Only roles inside this circle can view."


visibilityRestriction : String
visibilityRestriction =
    "User can join the organisation on invitation only."


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
    "Where Coordinator roles have the governance authority."


authAgile : String
authAgile =
    "Where all roles have equal governance authority."


labelsInfoHeader : String
labelsInfoHeader =
    "Creating new labels help you to sort out your tensions"


labelsInfoDoc : String
labelsInfoDoc =
    """
  When you create a label here, you can then attach it to your tensions. You will also be able to filter the tension according to labels.

  **Scope**: The labels that you can attach to a tension are those who are present in either its receicer circle or in any of the parents of the receiver circle.

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


peerRoleInfo : String
peerRoleInfo =
    "Basic Role with restricted authorization."


coordinatorRoleInfo : String
coordinatorRoleInfo =
    "Role with administration rights at the circle level."



--
-- letter
--


welcomeLetter : String
welcomeLetter =
    """
Welcome @{{username}},

**Your account has been successfully activated.**

Fractale is a platform for communities and organisations. We are working to provide you the best experience and tools to enact collective intelligence and bring collaboration to its next level.

You can also use it for individual projects to organise and manage your tasks and ideas.

In order to help your navigation, you will find some shortcuts in the top right corner:
* the **notification button** (<i class="icon icon-bell"></i>) to see and access your last notifications.
* the **help button** (<i class="icon icon-question"></i>) to access some documentation or contact us.
* the **new button** (<i class="icon icon-plus"></i>) to create new organisations.
* the **user button** to access your profile and settings.

"""


explainJoin : String
explainJoin =
    "Explain your motivation for joining this organisation ?"


orgaUserInvitation : String
orgaUserInvitation =
    "Users can join the organisation"


orgaUserInvitationHelp : String
orgaUserInvitationHelp =
    "User will be able to open a contract to join the organisation. The user will join as a Guest only after a coordinator validate the contract. For public organisation only."


guestCanCreateTension : String
guestCanCreateTension =
    "Guest can create tension"


guestCanCreateTensionHelp : String
guestCanCreateTensionHelp =
    "Guest are user that has been invited but do not play any role in the organisation."


notifyByEmail : String
notifyByEmail =
    "Enable email notifications."
