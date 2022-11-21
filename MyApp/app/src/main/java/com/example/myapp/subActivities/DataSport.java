package com.example.myapp.subActivities;

import android.app.DatePickerDialog;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.Button;
import android.widget.ListView;

import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.viewModal.DataSportViewModel;

import java.util.ArrayList;
import java.util.Calendar;

public class DataSport extends AppCompatActivity {

    DataSportViewModel dataSportViewModel;
    ListView listView;
    Button buttonDate;
    int year, month, day;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.data_sport);
        dataSportViewModel = new ViewModelProvider(this).get(DataSportViewModel.class);

        //get date from intent

        listView = findViewById(R.id.sportDataListView);

        SportDataListAdapter sportDataListAdapter = new SportDataListAdapter(this, new ArrayList<>());
        listView.setAdapter(sportDataListAdapter);
        dataSportViewModel.getTypeSportList().observeForever(typeSportList -> sportDataListAdapter.updateSportDataList(dataSportViewModel.processData(typeSportList)));

        getSupportActionBar().setDisplayHomeAsUpEnabled(true);

        Calendar currentDate = Calendar.getInstance();

        year = currentDate.get(Calendar.YEAR);
        month = currentDate.get(Calendar.MONTH);
        day = currentDate.get(Calendar.DAY_OF_MONTH);

        buttonDate = findViewById(R.id.buttonDate);
        buttonDate.setOnClickListener(view -> new DatePickerDialog(this, (datePicker, i, i1, i2) -> {
            year = i;
            month = i1;
            day = i2;
        }, year, month, day).show());
    }

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