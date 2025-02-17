with B;

package A is

  subtype T is B.T;

  function F (x : T) return T;
    
end A;
