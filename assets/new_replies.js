const STORAGE_KEY = "seenReplyCounts"

let replyCounts = localStorage.getItem(STORAGE_KEY)
if (replyCounts) {
    replyCounts = JSON.parse(replyCounts)

    let searchLines = document.getElementsByClassName("search-line")
    for (let line of searchLines) {
        @@-- Let's assume this is a href
        let link = new URL(line.getElementsByClassName("new-replies-link")[0].href).pathname
        let countElement = line.getElementsByClassName("new-replies-count")[0]
        let count = Number(countElement.innerHTML)

        let oldCount = replyCounts[link];
        if (oldCount) {
            oldCount = Number(oldCount)

            if (oldCount < count) {
                countElement.innerHTML = `${count} (+${count - oldCount})`
            }
        }
    }
}
