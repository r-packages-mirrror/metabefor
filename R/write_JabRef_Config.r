#' Write a JabRef configuration file
#' 
#' This function writes one or more JabRef configuration files that set
#' a number of fields to be visible to screeners, so that you can mask the
#' screeners from fields like journal, authors, and publication year.
#'
#' @param outputPath The path to write the file(s) to.
#' @param screeners The strings to use to identify the screeners.
#' @param screenerFieldsPrefix,screenerFieldsSuffix The prefix and suffix to
#' attach to the screener identification strings to create the fields where
#' the screeners store their decision.
#' @param screenerConfidencePrefix,screenerConfidenceSuffix The prefix and
#' suffix to attach to the screener identification strings to create the
#' fields where the screeners store their confidence in their decision.
#' @param fields The fields that have to be screened.
#' @param duplicateField The field 
#' @param sortField The field to sort entries by.
#' @param sortDesc Whether to sort in descending or ascending order.
#' @param screeningType The type of screening (added to the filenames).
#' @param jabrefVersion The version of JabRef to format the configuration file
#' for.
#' @param jabrefXML Optionally, the path to a file with a jabrefXML 'skeleton',
#' in which certain, as yet undocumented, replacements will be made (but you
#' can check the source code of the function in the meantime).
#' @param batFile Optionally, contents of the batch file to write.
#' @param generateSettingRemovalBatchFile Whether to write a batch file that
#' edits the Windows registry to remove JabRef configuration, which may
#' sometimes be necessary to get JabRef to use the XML configuration file.
#' @param screenerSuffixInFilename Whether to add the screener identifier as
#' a suffix to the file that is written.
#'
#' @return Invisibly, an object with the configuration file and intermediate
#' steps.
#' 
#' @export
#'
#' @examples ### Get the path to a temporary directory
#' tempPath <- tempdir();
#' 
#' ### Write screener packages
#' metabefor::write_JabRef_Config(
#'   tempPath,
#'   screeners = "A"
#' );
#' 
#' ### Load written file
#' jabRefConfig <-
#'   readLines(
#'     file.path(
#'       tempPath,
#'       "jabref.xml"
#'     )
#'   );
#' 
#' ### Show beginning of file
#' cat(jabRefConfig[1:10], sep="\n");
#' 
#' ### Show bit in the middle with specific screening info
#' cat(jabRefConfig[39:41], sep="\n");
write_JabRef_Config <- function(outputPath,
                                screeners = c("a", "b"),
                                screenerFieldsPrefix = "screener",
                                screenerFieldsSuffix = "status",
                                screenerConfidencePrefix = "screener",
                                screenerConfidenceSuffix = "confidence",
                                fields = c("title", "abstract"),
                                duplicateField = NULL,
                                sortField = "title",
                                sortDesc = FALSE,
                                screeningType = "screening",
                                jabrefVersion = "2.11.1",
                                jabrefXML = NULL,
                                batFile = NULL,
                                generateSettingRemovalBatchFile = FALSE,
                                screenerSuffixInFilename = FALSE) {

  res <- list(input = c(list(call = sys.call()),
                        as.list(environment())),
              intermediate = list(batFile = batFile),
              output = list());

  res$intermediate$screenerFields <-
    paste0(screenerFieldsPrefix, screeners, screenerFieldsSuffix);
  
  res$intermediate$screenerConfidenceFields <-
    paste0(screenerConfidencePrefix, screeners, screenerConfidenceSuffix);
  
  if (!is.null(duplicateField)) {
    res$intermediate$fields <- fields <- c(fields, duplicateField);
  }
  
  if (is.null(res$intermediate$batFile)) {
    res$intermediate$batFile <-
      paste0('java -jar JabRef-', jabrefVersion, '.jar --primp "SYSREV_XMLFILENAME"');
  }
  
  if (!is.null(jabrefXML)) {
    jabrefXML <- readLines(jabrefXML);
  } else {
    ### Generate basic Jabref configuration file
    if ((jabrefVersion == "2.10") || (jabrefVersion == "2.11.1")) {
      jabrefXML <- '<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE preferences SYSTEM "http://java.sun.com/dtd/preferences.dtd">
<preferences EXTERNAL_XML_VERSION="1.0">
  <root type="user">
    <map/>
    <node name="net">
      <map/>
      <node name="sf">
        <map/>
        <node name="jabref">
          <map>
            <entry key="memoryStickMode" value="true"/>
            <entry key="windowMaximised" value="true"/>
            <entry key="dialogWarningForDuplicateKey" value="false"/>
            <entry key="dialogWarningForEmptyKey" value="false"/>
            <entry key="useImportInspectionDialog" value="false"/>
            <entry key="useImportInspectionDialogForSingle" value="false"/>
            <entry key="enforceLegalBibtexKey" value="false"/>
            <entry key="confirmDelete" value="true"/>
            <entry key="allowTableEditing" value="false"/>
            <entry key="timeStampFormat" value="yyyy.MM.dd"/>
            <entry key="timeStampField" value="timestamp"/>
            <entry key="allowTableEditing" value="true"/>
            <entry key="defaultEncoding" value="UTF8"/>
            <entry key="backup" value="true"/>
            <entry key="autoSave" value="true"/>
            <entry key="autoSaveInterval" value="5"/>
            <entry key="defaultShowSource" value="false"/>
            <entry key="autoComplete" value="true"/>
            <entry key="autoCompleteFields" value="author;editor;title;journal;publisher;keywords;crossref;SYSREV_SCREENERFIELD"/>
            <entry key="fileColumn" value="false"/>
            <entry key="pdfColumn" value="false"/>
            <entry key="urlColumn" value="false"/>
            <entry key="citeseerColumn" value="false"/>
            <entry key="arxivColumn" value="false"/>
            <entry key="showSource" value="false"/>
            <entry key="preview0" value="\\begin{title} \\format[HTMLChars]{\\title} \\end{title}&lt;BR&gt;"/>
            <entry key="preview1" value="\\begin{title} \\format[HTMLChars]{\\title} \\end{title}&lt;BR&gt;"/>
            <entry key="customTabName_0" value="Screening"/>
            <entry key="customTabFields_0" value="SYSREV_FIELDS;SYSREV_SCREENERFIELD;SYSREV_CONF_FIELD"/>
            <entry key="columnNames" value="SYSREV_FIELDS;SYSREV_SCREENERFIELD"/>
            <entry key="columnWidths" value="SYSREV_FIELDWIDTHS"/>
            <entry key="numberColWidth" value="50"/>
            <entry key="priSort" value="SYSREV_SORTFIELD"/>
            <entry key="priDescending" value="SYSREV_SORTDESCENDING"/>
            <entry key="entryEditorHeight" value="600"/>
            <entry key="previewPanelHeight" value="0"/>
            <entry key="customTypeName_0" value="SYSREV_SCREENINGTYPE"/>
            <entry key="customTypeReq_0" value=""/>
            <entry key="customTypeOpt_0" value="SYSREV_FIELDS"/>
            <entry key="customTypePriOpt_0" value="SYSREV_FIELDS"/>
          </map>
          <node name="labelPattern">
            <map/>
          </node>
        </node>
      </node>
    </node>
  </root>
</preferences>';
    }
    else {
      stop("The basic configuration file for this version of JabRef is not yet implemented!");
    }
  }
  
  res$intermediate$screenerXML <- list()
  for (currentScreener in 1:length(screeners)) {
    
    res$intermediate$screenerXML[[currentScreener]] <- jabrefXML;
    
    ### Replace screenerfield for this screener
    if (!is.null(screenerConfidencePrefix)) {
      res$intermediate$screenerXML[[currentScreener]] <-
        gsub("SYSREV_CONF_FIELD",
             res$intermediate$screenerConfidenceFields[currentScreener],
             res$intermediate$screenerXML[[currentScreener]]);
    } else {
      res$intermediate$screenerXML[[currentScreener]] <-
        gsub("SYSREV_CONF_FIELD",
             "",
             res$intermediate$screenerXML[[currentScreener]]);
    }
    res$intermediate$screenerXML[[currentScreener]] <-
      gsub("SYSREV_SCREENERFIELD",
           res$intermediate$screenerFields[currentScreener],
           res$intermediate$screenerXML[[currentScreener]]);

    ### Replace fields to screen
    res$intermediate$screenerXML[[currentScreener]] <-
      gsub("SYSREV_FIELDS", 
           paste0(fields,
                  collapse=";"),
           res$intermediate$screenerXML[[currentScreener]]);

    ### Replace field widths
    fieldWidths <- paste0(paste(round(rep(1000/length(fields), length(fields))), collapse=";"), ";100");
    res$intermediate$screenerXML[[currentScreener]] <- gsub("SYSREV_FIELDWIDTHS", fieldWidths, res$intermediate$screenerXML[[currentScreener]]);
    
    ### Replace fields to screen
    res$intermediate$screenerXML[[currentScreener]] <- gsub("SYSREV_SCREENINGTYPE", screeningType, res$intermediate$screenerXML[[currentScreener]]);

    ### Replace field to sort on and direction of sorting
    res$intermediate$screenerXML[[currentScreener]] <- gsub("SYSREV_SORTFIELD", sortField, res$intermediate$screenerXML[[currentScreener]]);
    res$intermediate$screenerXML[[currentScreener]] <- gsub("SYSREV_SORTDESCENDING", tolower(as.character(sortDesc)), res$intermediate$screenerXML[[currentScreener]]);
    
    if (screenerSuffixInFilename) {
      filenameSuffix <- paste0("--", screeners[currentScreener]);
    }
    else {
      filenameSuffix <- "";
    }
    ### Write config file
    writeLines(res$intermediate$screenerXML[[currentScreener]],
               file.path(
                 outputPath,
                 paste0("jabref", filenameSuffix, ".xml"))
               );
    ### Write batch file to open JabRef with config file
    writeLines(gsub("SYSREV_XMLFILENAME",
                    paste0("jabref", filenameSuffix, ".xml"),
                    res$intermediate$batFile),
               file.path(outputPath,
                         paste0("screen", filenameSuffix, ".bat"))
               );
    
    ### Is requested, write registry file and batch file to remove settings
    if (generateSettingRemovalBatchFile) {
      writeLines("REGEDIT4\n[-HKEY_CURRENT_USER\\Software\\JavaSoft\\Prefs\\net\\sf\\jabref]",
                 paste0(outputPath, "/remove-jabref-settings-windows-only.reg"));
      writeLines('regedit /S "remove-jabref-settings-windows-only.reg"',
                 paste0(outputPath, "/remove-jabref-settings--windows-only.bat"));
    }
  }
  
  return(invisible(res));

}

