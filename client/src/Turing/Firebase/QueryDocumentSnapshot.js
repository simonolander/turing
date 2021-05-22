"use strict";

// See https://firebase.google.com/docs/reference/js/firebase.firestore.QueryDocumentSnapshot#data
exports._data = function (queryDocumentSnapshot) {
    return function (snapshotOptions) {
        return function () {
            return queryDocumentSnapshot.data(snapshotOptions);
        }
    }
}
