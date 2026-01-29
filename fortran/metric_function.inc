      if (i.eq.1) then
       if (j.eq.1) then
        lll=1.000000000000000d0
       else if (j.eq.2) then
        lll=0.000000000000000d0
       endif
      else if (i.eq.2) then
       if (j.eq.1) then
        lll=0.000000000000000d0
       else if (j.eq.2) then
        lll=Exp(16.d0*x(1))
       endif
      endif

