package com.example.myapp.subActivities;

import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.viewModal.DataMusicViewModel;
import com.google.android.material.textfield.TextInputLayout;

import java.util.ArrayList;
import java.util.Objects;

public class DataMusic extends AppCompatActivity {

    DataMusicViewModel dataMusicViewModel;
    ListView songSelected, songUnselected;
    Button editSaveButton, returnButton;
    TextInputLayout playlistNameInput;
    EditText playlistName;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.data_music);
        dataMusicViewModel = new ViewModelProvider(this).get(DataMusicViewModel.class);
        //get playlist id from intent first
        //dataMusicViewModel.populateLists();
        Objects.requireNonNull(getSupportActionBar()).setDisplayHomeAsUpEnabled(true);
        initialiseAll();
    }

    public void initialiseAll(){
        initialiseListViews();
        initialiseEditText();
        initialiseButton();
    }

    public void initialiseListViews(){
        songUnselected = findViewById(R.id.songUnselected);
        songSelected = findViewById(R.id.songSelected);

        MusicDataListAdapter songUnselectedAdapter = new MusicDataListAdapter(this, R.layout.data_music_list_item, new ArrayList<>());
        songUnselected.setAdapter(songUnselectedAdapter);
        dataMusicViewModel.getUnselectedSongs().observeForever(songUnselectedAdapter::updateSongList);

        MusicDataListAdapter songSelectedAdapter = new MusicDataListAdapter(this, R.layout.data_music_list_item, new ArrayList<>());
        songSelected.setAdapter(songSelectedAdapter);
        dataMusicViewModel.getSelectedSongs().observeForever(songSelectedAdapter::updateSongList);
    }

    public void initialiseEditText(){
        playlistNameInput = findViewById(R.id.playlistNameInput);
        playlistName = findViewById(R.id.playlistName);
        playlistName.addTextChangedListener(playlistNameTextWatcher);
        playlistName.setOnFocusChangeListener((v, hasFocus) -> validatePlaylistName(playlistNameInput, playlistName));
    }

    public void initialiseButton(){
        editSaveButton = findViewById(R.id.editSaveButton);
        returnButton = findViewById(R.id.returnButton);
        returnButton.setOnClickListener(v -> finish());
    }

    public boolean validatePlaylistName(TextInputLayout textInputLayout, EditText editText){
        String playlistText = editText.getText().toString();
        boolean hasFocus = editText.hasFocus();
        boolean emptyUsername = playlistText.isEmpty();
        boolean validUsername = !emptyUsername && dataMusicViewModel.validatePlaylistName(playlistText);

        if(!hasFocus || validUsername)
            textInputLayout.setErrorEnabled(false);
        else if(emptyUsername)
            textInputLayout.setError("Playlist name cannot be empty");
        else
            textInputLayout.setError("Playlist name already taken");
        return validUsername;
    }

    private final TextWatcher playlistNameTextWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            boolean validPlaylistName = validatePlaylistName(playlistNameInput, playlistName);
            editSaveButton.setEnabled(validPlaylistName);
        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    };

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            finish();
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    public boolean onCreateOptionsMenu(Menu menu) {
        return true;
    }
}