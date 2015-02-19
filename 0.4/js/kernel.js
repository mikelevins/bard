function kernelEvalApplication(expr, env) {
    return null;
};
function kernelEval(expr, env) {
    if (expr == null) {
        return null;
    } else if ("undefined" === typeof expr) {
        return undefined;
    } else if (typeof expr === "string" || typeof expr === "number" || typeof expr === "function") {
        return expr;
    } else if (typeof expr === "object") {
        return Array === expr.constructor ? kernelEvalApplication(expr, env) : expr;
    } else {
        return expr;
    };
};
