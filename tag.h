// ======================================================================
// tag
// ======================================================================
// tags in Bard are stored in the bottom byte of an integer.
// an integer in Bard is always one half of a cell--either the
// car or the cdr.
// a tag operation therefore always operates on the lowest-order
// byte of the car or cdr field of a cell. Any other operation
// on a value first shifts the car or cdr to eliminate the tag,
// then operates on the resulting value, then shifts the
// resulting value and replaces its low-order byte with the
// proper tag.

