# Feature Plan

- [ ] (Feature) Property packs: a way to add a pack of properties
      to the type-level descriptions with only one line of code.
      Example:

      ```haskell
      type Door ... = PropDict
           '[ Pack Openable      -- adds open/close states, state val and scripts
           ]
      ```

- [ ] (Mechanics) World type-level description
      & objects position integration

