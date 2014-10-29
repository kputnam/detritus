module Math.Complex
  where

-- Every complex number can be written in the form
--
--    Z = a + b*i
--      where a, b :: Real
--
--    Z = r * e^(theta * i)
--      where r, theta :: Real
--
--      theta is the angle between x-axis and "argument" of Z
--      so e^(theta * i) lies along unit circle, and r scales
--      this vector along its direction (r is the the "absolute
--      value" of Z)
--
-- This seems a lot like polar and cartesian coordinates
--
--    Due to e^a * e^b = e^(a+b)
--      re^(theta*i) * e^(tau*i) = re^((theta+tau)*i)
--    so f(z) = e^(tau*i) rotates z by angle tau
