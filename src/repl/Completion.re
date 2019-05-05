let complete = input => {
  VendoredUTop.UTop_complete.complete(
    ~syntax=Normal, 
    ~phrase_terminator=SyntaxControl.currentSeperator(), 
    ~input
  );
};
