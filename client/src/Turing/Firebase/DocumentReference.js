"use strict";

exports.get = function (documentReference) {
    return function (options) {
        return function () {
            return documentReference.get(options);
        }
    }
};

exports.set = function (documentReference) {
    return function (data) {
        return function () {
            return documentReference.set(data, {});
        };
    };
};
