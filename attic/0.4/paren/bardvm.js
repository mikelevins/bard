function k_typeOf(thing) {
    return thing['type'];
};
var nothing = [];
var end = { 'type' : 'end' };
function k_cons(left, right) {
    return { 'type' : 'cons',
             'car' : left,
             'cdr' : right
           };
};
function k_car(thing) {
    return thing['car'];
};
function k_cdr(thing) {
    return thing['cdr'];
};
function k_keys(m) {
    var entries = m['entries'];
    return Object.keys(entries);
};
function k_empty_map() {
    return { 'type' : 'map', 'entries' : {  } };
};
function k_contains_key(m, k) {
    var entries = m['entries'];
    var entryKeys = Object.keys(entries);
    return entryKeys.includes(k);
};
function k_get_key(m, k, defaultVal) {
    if (k_contains_key(m, k)) {
        var entries = m['entries'];
        return entries[k];
    } else {
        return defaultVal;
    };
};
function k_set_key(m, k, val) {
    var entries = m['entries'];
    entries[k] = val;
    return m;
};
