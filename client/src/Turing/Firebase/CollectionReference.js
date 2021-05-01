"use strict";

exports.doc = function (collectionReference) {
    return function (documentPath) {
        return function () {
            return collectionReference.doc(documentPath);
        };
    };
};
