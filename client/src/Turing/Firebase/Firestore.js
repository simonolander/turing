"use strict";

exports.firestore = function () {
    return firebase.firestore();
};

exports.collection = function (firestore) {
    return function (collectionPath) {
        return function () {
            return firestore.collection(collectionPath);
        };
    };
};
