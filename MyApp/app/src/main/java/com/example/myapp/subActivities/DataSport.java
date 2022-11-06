package com.example.myapp.subActivities;

import android.app.DatePickerDialog;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.Button;
import android.widget.DatePicker;
import android.widget.ListView;

import androidx.appcompat.app.AppCompatActivity;

import com.example.myapp.R;
import com.example.myapp.subActivities.listSportData.SportDataListAdapter;
import com.example.myapp.subActivities.listSportData.SportDataListItem;

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

public class DataSport extends AppCompatActivity {

    int year, month, day;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.data_sport);

        ListView listView = findViewById(R.id.sportDataListView);
        List<SportDataListItem> sportDataListItemList = new ArrayList<>();

        sportDataListItemList.add(new SportDataListItem("test", 0, 0));
        sportDataListItemList.add(new SportDataListItem("test1", 1, 1));
        sportDataListItemList.add(new SportDataListItem("test", 0, 0));
        sportDataListItemList.add(new SportDataListItem("test1", 1, 1));
        sportDataListItemList.add(new SportDataListItem("test", 0, 0));
        sportDataListItemList.add(new SportDataListItem("test1", 1, 1));

        SportDataListAdapter sportDataListAdapter = new SportDataListAdapter(this, R.layout.data_sport_list_item, sportDataListItemList);
        listView.setAdapter(sportDataListAdapter);

        getSupportActionBar().setDisplayHomeAsUpEnabled(true);

        Calendar currentDate = Calendar.getInstance();

        year = currentDate.get(Calendar.YEAR);
        month = currentDate.get(Calendar.MONTH);
        day = currentDate.get(Calendar.DAY_OF_MONTH);

        Button buttonDate = findViewById(R.id.buttonDate);
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