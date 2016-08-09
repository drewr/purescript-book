"use strict";

exports.alert = function(msg) {
    return function() {
        window.alert(msg);
        return {};
    };
};

exports.confirm = function(msg) {
    return function() {
        window.confirm(msg);
        return {};
    };
};
