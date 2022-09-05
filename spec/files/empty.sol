// SPDX-License-Identifier: GPL-3.0
pragma solidity >=0.4.16 <0.9.0;

contract SimpleStorage {
    function set(uint x) public {
      string name = "haskell";
      uint amount;
      /* and a 
         little block */
      while (4 != (8 - 4)) {
        break;
      }
      /* { */
      /*   if (eh) { */
      /*   } */
      /* } */
      do {
          print();
      } while (true != false);
    }

    // This is a comment
    function get() public view returns (uint) {
      if (whatever) {
        while (false) {}
      } else {
        if (false) {
          continue;
        }
      }

      for (;;) {
        return;
      }
      for (uint i = 0; i < proposalNames.length; i++) {}

      return 4 ** 2;
    } 
}

interface AnInterfaceThing is Parent, SuperParent {
    function doSomethingSweet();
}
