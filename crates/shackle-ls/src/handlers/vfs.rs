use std::path::Path;

use lsp_types::{
	DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
};

use crate::LanguageServerDatabase;

pub fn on_document_open(db: &mut LanguageServerDatabase, params: DidOpenTextDocumentParams) {
	let file = params.text_document.uri.as_str();
	db.manage_file(Path::new(file), &params.text_document.text);
}

pub fn on_document_changed(db: &mut LanguageServerDatabase, params: DidChangeTextDocumentParams) {
	let file = params.text_document.uri.as_str();
	db.manage_file(
		Path::new(file),
		&params
			.content_changes
			.iter()
			.map(|c| c.text.clone())
			.collect::<String>(),
	);
}

pub fn on_document_closed(db: &mut LanguageServerDatabase, params: DidCloseTextDocumentParams) {
	let file = params.text_document.uri.as_str();
	db.unmanage_file(Path::new(file));
}
