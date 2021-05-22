"use strict";

// See https://firebase.google.com/docs/reference/js/firebase.firestore.DocumentReference#get
exports.get = function (documentReference) {
    return function (options) {
        return function () {
            return documentReference.get(options);
        }
    }
};

// See https://firebase.google.com/docs/reference/js/firebase.firestore.DocumentReference#set
exports.setImpl = function (documentReference) {
    return function (data) {
        return function () {
            return documentReference.set(data, {});
        };
    };
};
