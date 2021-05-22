"use strict";

// See https://firebase.google.com/docs/reference/js/firebase.firestore.DocumentSnapshot#id
exports.id = function (documentSnapshot) {
    return documentSnapshot.id
}

// See https://firebase.google.com/docs/reference/js/firebase.firestore.DocumentSnapshot#exists
exports.exists = function (documentSnapshot) {
    return documentSnapshot.exists
}

// See https://firebase.google.com/docs/reference/js/firebase.firestore.DocumentSnapshot#data
exports._data = function (documentSnapshot) {
    return function (options) {
        return function () {
            // I found no great way to deal with undefined values,
            // so I'm converting undefined to null for the moment.
            var data = documentSnapshot.data(options);
            if (typeof data !== "undefined") {
                return data;
            }
            else {
                return null;
            }
        };
    };
};
