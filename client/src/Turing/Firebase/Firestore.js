"use strict";

// See https://firebase.google.com/docs/reference/js/firebase.firestore#callable
exports.firestore = function () {
    return firebase.firestore();
};

// See https://firebase.google.com/docs/reference/js/firebase.firestore.Firestore#collection
exports.collection = function (firestore) {
    return function (collectionPath) {
        return function () {
            return firestore.collection(collectionPath);
        };
    };
};
