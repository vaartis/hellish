{
    const ELEMENT_CLASS = "utc-date";

    let dateBlocks = document.getElementsByClassName(ELEMENT_CLASS);
    for (let dateBlock of dateBlocks) {
        let date = new Date(dateBlock.innerHTML + "+00");
        date.setMinutes(date.getMinutes() - date.getTimezoneOffset());

        let isoString = date.toISOString();
        let [dateStr, timeStr] = isoString.split("T");
        timeStr = timeStr.split(".")[0];

        dateBlock.innerHTML = `${dateStr} ${timeStr}`;
    }
}
