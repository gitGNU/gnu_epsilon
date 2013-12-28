/* This file is part of GNU epsilon.

   Copyright (C) 2012 Université Paris 13
   Written by Luca Saiu

   GNU epsilon is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   GNU epsilon is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU epsilon.  If not, see <http://www.gnu.org/licenses/>. */


/* This header does not prevent mutliple #inclusion, and it's meant not to. */

// To do: disable this *and don't use these features* in production.
/* Ugly kluge, useful for debugging; this makes some stdio functions
   signal-safe. We #define these here because we want these to be
   as visible as possible, and this header is always #include'd at the
   beginning. */
#ifdef ENABLE_ASSERTIONS
/* Note that disabling this hack amounts to assuming that absolutely
   *nothing* is fprintf()ed while collecting on the same streams that
   mutators may be using.
   Such an assumption is reasonable for production, but not for
   debugging. */
#define printf epsilongc_signal_safe_printf
#define fprintf epsilongc_signal_safe_fprintf
#define fflush epsilongc_signal_safe_fflush
#endif // #ifdef ENABLE_ASSERTIONS
