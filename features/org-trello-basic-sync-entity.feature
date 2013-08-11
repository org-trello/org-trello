Feature: Synchronizing basic entity

  Scenario: item synchro failure
    When I insert:
    """
    * card
    ** checklist
    *** item 0
    *** item 1
    """
    And I press "C-c o c"
    Then I should not have a property "orgtrello-id"

  Scenario: checklist synchro failure
    When I insert:
    """
    * card
    ** checklist
    *** item 0
    *** item 1
    """
    And I go to the word "checklist"
    And I press "C-c o c"
    Then I should not have a property "orgtrello-id"

  Scenario: card synchro success
    When I insert:
    """
    * card
    ** checklist
    *** item 0
    *** item 1
    """
    And I go to the word "card"
    And I press "C-c o c"
    Then I should have a property "orgtrello-id"

  Scenario: card + checklist synchro success
    When I insert:
    """
    * card
    ** checklist
    *** item 0
    *** item 1
    """
    And I go to the word "card"
    And I press "C-c o c"
    And I go to the word "checklist"
    And I press "C-c o c"
    Then I should have a property "orgtrello-id"

  Scenario: card + checklist + item synchro success
    When I insert:
    """
    * card
    ** checklist
    *** item 0
    *** item 1
    """
    And I go to the word "card"
    And I press "C-c o c"
    And I go to the word "checklist"
    And I press "C-c o c"
    And I go to the word "item 0"
    And I press "C-c o c"
    Then I should have a property "orgtrello-id"

  Scenario: Synchronize an entity with level more than 4 is not dealt with
    When I insert:
    """
    * card
    ** checklist
    *** item 0
    **** entity with level 4 and more are not sync

    """
    And I go to the word "* card"
    And I press "C-c o c"
    Then I should have a property "orgtrello-id"
    And I go to the word "** checklist"
    And I press "C-c o c"
    Then I should have a property "orgtrello-id"
    And I go to the word "*** item 0"
    And I press "C-c o c"
    Then I should have a property "orgtrello-id"
    And I go to the word "**** entity with level 4"
    And I press "C-c o c"
    And I go to the word "**** entity with level 4"
    Then I should not have a property "orgtrello-id"
