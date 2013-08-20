Feature: Synchronize complex entity

  Scenario: Synchronize full card
    When I insert:
    """
    * card
    ** checklist
    *** item 0
    *** item 1
    """
    And I go to the word "* card"
    And I press "C-c o C"
    Then I should have a property "orgtrello-id"
    And I go to the word "** checklist"
    Then I should have a property "orgtrello-id"
    And I go to the word "*** item 0"
    Then I should have a property "orgtrello-id"
    And I go to the word "*** item 1"
    Then I should have a property "orgtrello-id"

  Scenario: Synchronize full checklist
    When I insert:
    """
    * card
    ** checklist
    *** item 0
    *** item 1
    """
    And I go to the word "* card"
    And I press "C-c o c"
    Then I should have a property "orgtrello-id"
    And I go to the word "** checklist"
    And I press "C-c o C"
    Then I should have a property "orgtrello-id"
    And I go to the word "*** item 0"
    Then I should have a property "orgtrello-id"
    And I go to the word "*** item 1"
    Then I should have a property "orgtrello-id"

  Scenario: Synchronize full item, only this item is synced
    When I insert:
    """
    * card
    ** checklist
    *** item 0
    *** item 1
    """
    And I go to the word "* card"
    And I press "C-c o c"
    Then I should have a property "orgtrello-id"
    And I go to the word "** checklist"
    And I press "C-c o c"
    Then I should have a property "orgtrello-id"
    And I go to the word "*** item 0"
    And I press "C-c o C"
    Then I should have a property "orgtrello-id"
