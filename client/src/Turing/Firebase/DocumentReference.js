"use strict";

exports.get = function (documentReference) {
    return function (options) {
        return function () {
            return documentReference.get(options);
        }
    }
};
