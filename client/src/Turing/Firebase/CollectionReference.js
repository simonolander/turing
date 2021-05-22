"use strict";

// See https://firebase.google.com/docs/reference/js/firebase.firestore.CollectionReference#doc
exports.doc = function (collectionReference) {
    return function (documentPath) {
        return function () {
            return collectionReference.doc(documentPath);
        };
    };
};

// See https://firebase.google.com/docs/reference/js/firebase.firestore.CollectionReference#get
exports.get = function (collectionReference) {
    return function (options) {
        return function () {
            return collectionReference.get(options);
        };
    };
};
